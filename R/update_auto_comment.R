
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
  blocks_with_auto_comment_tag<-purrr::keep(
    blocks,
    function(block){'autocomment' %in% block_tags(block)}
  )
  blocks_with_auto_comment_tag
}

roclet_output.roclet_update_auto_comment<- function(x, results, base_path, ...) {
  # Handle the blocks that have an autocomment tag and check if the content of the comments in this block
  # is still up to date. At the moment we only do this for s4 methods and generics and also only for the 
  # @param tag. If a parameter is removed from the code we also remove it from the comment. If a new 
  # (undocumented)  param is found we add a minimal @param to the comment 
  #first filter out the blocks we want to do something about
  # at the moment just s4mehtods and s4generics
  cls<-c('s4method','s4generic')
  results<-purrr::keep(
    results,
    function(block){
      obj<-block$value
      any(vapply(
        cls,
        function(cl){inherits(obj,cl)},
        FUN.VALUE=logical(1)
      ))
    }
  )
  browser() 
  files<-unique(lapply(results,function(block) block$file))

  for (file in files){
    lines<-read_lines(file)
    # find the blocks 
    bs<-purrr::keep(
      results,
      function(block){
        block$file==file
      }
    )
    lapply(
      bs,
      function(block){
        # find @param tags
        param_tags<- purrr::keep(
          block$tags,
          .p=function(tag){tag$tag=='param'}
        )

        documented_args<-lapply(
          param_tags,
          function(tag) tag$val$name
        )
        # find the params in the code 
        function_args<-names(formals(block$object$value))
        
        # look for superflous comments
        wrong_names=setdiff(documented_args,function_args)
        
        wrong_tags<-purrr::keep(
          param_tags,
          function(tag){tag$val$name %in% wrong_tags}
        )
        wrong_lines<-unlist(lapply(
          wrong_tags,
          function(tag){
            start<-tag$line
            end<-stop('not implemented yet,find the line of the next comment find all the lines of the other tags in the block, order them and ')
            seq(start,end)
          }
        ))
        #remove those lines

        # find as yet undocumented names
        undocumented_args<-setdiff(function_args,documented_args)
        stop('add the comment lines as the auto_comment_roclet')

      }
    )
    browser()
  }
}
