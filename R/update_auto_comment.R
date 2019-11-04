#' Roclet: Update the blocks that have an 'autocomment' tag.
#' 
#' @description 
#' This roclet finds all roxygen comment blocks that contain 
#' an @autocomment tag, checks if the comments are 
#' consistent with the documented code and possibly adapts 
#' the comments accordingly.
#' The automatically updated documenting comments only contain
#' information that can be inferred by introspection of the code.
#' This is useful for the following reasons:
#' \enumerate{
#' \item For some object, especially generic functions, mehtods and classes the automatically created documentation can be helpful for the revealing of the code structure allone. 
#' \item The automatically created comments can be extended manually.
#' \item The automatically created documentation will pass the R CMD check test immediatly. 
#' }
#' It is thus usefull to achieve and maintain a minimal but cran ready documentaion quickly that can be extended at libitum. 
#' 
#' At the moment we only do this for S4 methods and generics 
#' and also only for the @param tag. 
#' \enumerate{
#'    \item If a parameter is removed from the code we also remove it from the comment. 
#'    \item If a parameter is removed from the code we also remove it from the comment. 
#'    If a new (undocumented) param is found in the code we add a minimal @param 
#'    to the comment.
#' }
#' Future versions of the roclet could extend this functionality 
#' in two ways:
#' \enumerate{
#' \item Extend the set of objects on which to operate, e.g by including functions, r6-methods ... 
#' \item Extend the set of tags that can be changed, e.g. slots for 
#' classes, ...
#' }
#' @export
#' @family roclets
update_auto_comment_roclet<- function() {
  roclet("update_auto_comment")
}

#' @export
roxy_tag_parse.roxy_tag_autocomment <- function(x) {
  tag_words_line(x)
}

roxy_tag_rd.roxy_tag_autocomment<- function(x, base_path, env) {
  value <- NULL
  rd_section(x$tag, value)
}

#' @export
roclet_process.roclet_update_auto_comment<- function(x, blocks, env, base_path) {
  # Filter out the blocks with the @autocomment tag 
  ba<-purrr::keep(
    blocks,
    function(block){'autocomment' %in% block_tags(block)}
  )
  
  # keep  only s4mehtods and s4generics. 
  # It is possible to extend the roclet to handle other objects as well but this is
  # not implemented yet.
  clvs<-list(c('s4method','object'),c('s4generic','object'))
  results<-purrr::keep(
    ba,
    function(block){
      any(
        as.logical(
          lapply(
            clvs,
            function(clv) all(class(block$object)==clv)
          )
        )
      )
    }
  )
  results
}



roclet_output.roclet_update_auto_comment<- function(x, results, base_path, ...) {
  files<-unique(lapply(results,function(block) block$file))
  lapply(files,update_file_autocomments,results)
  invisible()
}

# helper
update_block_autocomments<-function(block,lines){
  # find all @param tags
  param_tags<- purrr::keep(
    block$tags,
    .p=function(tag){tag$tag=='param'}
  )
  if (length(param_tags)<1){
    new_lines <- lines
  }else{
    # some tags that we want to change may span several lines
    # to know which lines have to change we need an
    # overview where the `next` after the one we change starts.
    # To this end we need an overview 
    sorted_tag_lines<-sort(
      unlist(
        purrr::discard(
          lapply(
            purrr::discard(
              block$tags,
              function(tag){
              # fixme:
              # for some reason the title tag yields has the line attribute 
              # always set to 1 for all blocks which is certainly wrong if
              # interpreted as a line number in the src file 
              # but I do not know if it serves another purpose
              tag$tag=='title'
              }
            ),
            function(tag){tag$line}
          ),
          function(line) is.na(line)
        )
      )
    )
  
    remove<-unlist(
      lapply(
        param_tags,
        function(tag){
          first_line<-tag$line
          last_line<-sorted_tag_lines[[match(tag$line,sorted_tag_lines)+1]]-1
          seq(first_line,last_line)
        }
      )
    )
    lines<-lines[
      purrr::discard(
        seq_along(lines),
        function(nr) nr %in% remove
      )
    ]
    #s_par_tags<-param_tags[order(pos,decreasing=TRUE)]
    #for (tag in s_par_tags){ 
    #      first_line<-tag$line
    #      last_line<-sorted_tag_lines[[match(tag$line,sorted_tag_lines)+1]]-1
    #      browser()
    #      lines<-lines[setdiff(seq_along(lines),first_line:last_line)]

    #    #remove those lines
    # }
    
    documented_args<-lapply(
      param_tags,
      function(tag) tag$val$name
    )
    # inspect the params in the code 
    function_args<-names(formals(block$object$value))
    
    # keep only the still valid param tags  
    valid_param_tags<-purrr::keep(
      param_tags,
      function(tag) tag$val$name %in% function_args
    )
    # find as yet undocumented names
    undocumented_args<-setdiff(function_args,documented_args)
    # create tags for the undocumented args
    extra_tags<-extra_param_tags(block$object,undocumented_args)

    complete_param_tags<-append(
      valid_param_tags,
      extra_tags
    )
    # fixme: order them in the order of the formals
    param_lines<-unlist(
      lapply(
        complete_param_tags,
        param_tag_to_block_lines
      )
    )
    # now inject the comment lines for all params into the file
    # at the position where the first @param comment of the block
    # had been
    pos<-as.numeric(lapply(param_tags,function(tag) tag$line))
    line_nr<-min(pos)
    new_lines<-c(
      lines[1:line_nr],
      param_lines,
      lines[(line_nr+1):length(lines)]
    )
  }
  browser()
 new_lines
}

#helper
extra_param_tags<- function(object, names) {
  UseMethod("extra_param_tags")
}

extra_param_tags.s4method<-function(object,names){
  sig<-object$value@defined
  lapply(
    names,
    function(name,object){
      description<-'not manually documented yet , found by inspection'
      if (name %in% names(sig)){
        d <- paste("object of class:", "\\code{",sig[[name]],"}",'. ' ,description,sep='')
        tag <- roxy_tag(
          tag='param',
          raw=paste0(name,': ',d,sep=''),
          val=list("name"=name,description=d)
        )
      }else{
        d<-description
      }
      roxy_tag(
        tag='param',
        raw=paste(name,': ',d,sep=''),
        val=list("name"=name,description=d)
      )
    }
  )
}
extra_param_tags.s4generic<-function(object,names){
  lapply(
    names,
    function(name) {
      d<-'not manually documented, see method documentation'
      roxy_tag(
        tag='param',
        raw=paste(name,': ',d,sep=''),
        val=list("name"=name,description=d)
      )
    }
  )
}

# helper
update_file_autocomments<-function(file,results){
  lines<-read_lines(file)
  # find the blocks 
  bs<-purrr::keep(
    results,
    function(block){
      block$file==file
    }
  )
  # order the blocks in the REVERSE order of 
  # their appearance in the file
  bs<-bs[
    order(
      as.numeric(lapply(bs,function(b){b$line})),
      decreasing=TRUE
    )
  ]
  # We start changing the comment lines of the last block
  # and work our way backwards to the first block
  # Thus only the positions of the already updated blocks in
  # the file change
  for (block in bs){
      lines<-update_block_autocomments(block,lines)
      # find the start lines of all the tags in the block (except the title tag)
      #write_lines(lines,file)
  } 
}

# helper
param_tag_to_block_lines<-function(tag){
  value<-tag$val
  name<-value$name
  d   <-value$description
  line<-comment_tag(
    tag='@param',
    value=paste0(name,': ',d)
  )
  line
}
