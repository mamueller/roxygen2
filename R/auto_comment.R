auto_comment_roclet <- function() {
  roclet("auto_comment")
}


## copied from the rd2roxygen package
## wrap strings with comment prefix
comment_line = function(x, exdent = 0) {
  if (missing(x)) return(comment_prefix())

  strwrap(x, width = 80, exdent = exdent, prefix = comment_prefix())
}

## add comments
## copied from the rd2roxygen package
comment_tag = function(tag, value) {
  value = value[value != '']
  if (length(value) == 0) return()

  comment_line(paste(tag, value), exdent = 0)
}

## access the comment prefix
## copied from the rd2roxygen package
comment_prefix = function() {
  getOption("roxygen.comment", "#' ")
}

object_to_block_lines <- function(object){ 
  UseMethod("object_to_block_lines")
}
auto_comment_tag_lines <- comment_tag("@autocomment ",' These comments were created by the auto_comment_roclet by inspection of the code. 
You can use the "update_auto_comment_roclet" to automatically adapt them to changes in the source code.
This will remove `@param` tags for parameters that are no longer present in 
the source code and add `@param` tags with a default description for yet undocumented parameters. 
If you remove this `@autocomment` tag your comments will no longer be touched by the "update_autocomment_roclet".')


object_to_block_lines.s4generic<-function(object){
  res <-c(
    comment_line(paste0("S4 generic: ",object$topic)), #title
    comment_line(),
    comment_tag("@name",object$topic),
    as.character(
      lapply(
        names(formals(object$value)),
        function(arg){
          comment_tag(
            tag='@param',
            value=paste0(arg," see method arguments")
          )
        }
      )
    ),
    comment_tag("@s4methods",' '),
    auto_comment_tag_lines
  )
  res
}

object_to_block_lines.s4method<-function(object){
  extra_tags <- extra_param_tags(object)
  param_lines<- unlist(
    lapply(
      extra_tags,
      param_tag_to_block_lines
    )
  )
  res <-c(
    comment_line(paste0("Automatic description: ",object$topic)), #title
    comment_line(),
    comment_tag("@name",object$topic),
    param_lines,
    auto_comment_tag_lines
  )
  res
}

#' @export
roclet_process.roclet_auto_comment<- function(x, blocks, env, base_path) {
  # The list of blocks that this function receives as an argument is incomplete
  # since some of the objects have no roxygen comments yet but are going to
  # have some automatically created by this roclet.
  # To find them we have to use R's own parse function. 
  #env <- env_package(base_path)
  files <- package_files(base_path)
  a_o<- lapply(
    files,
    function(file){
      sf<-srcfile(file)
      exprs<-parse(file,keep.source=TRUE,srcfile=sf)
      if(length(exprs)==0){
        return(list())
      }else{
        objects_from_file<-purrr::keep(
          lapply(
            1:length(exprs),
            function(i){
              call<-exprs[[i]]
              # fixme:
              # create a pseudo block to be able to call object_from_call with
              # a block argument. 
              # This is a bit of "cargo cult programming" and could be avoided
              # if object_from_call would be changed a little bit
              # at the moment I want to be as least invasive
              # as possible though and integrate it in case the pull request 
              # is accepted
              b<-roxy_block(tags=list(),file=file,line=5000L,call=call,object=NULL)
              
              # some calls like globalVariables(c(something))
              # are not recognized. So we have to make sure
              # that we do not try to attach a "srcref" 
              # attribute to them
              o<-tryCatch(
                object_from_call( call=call, env=env,block=b, file=file),
                error=function(e){
                  NULL
                }
              )
              if (!is.null(o)){attr(o,"srcref")<-attr(exprs,'srcref')[[i]]}
              o
            }
          ),
          .p=function(el){!is.null(el)}
        )
      }
    }
   )

  a_o_flat <- list()
  for(o in a_o) {a_o_flat<-append(a_o_flat,o)}

  objectsWithBlocks<-lapply(blocks,function(block){block$object}) 
  # find the undocumented objects
  untagged_objects<-setdiff(a_o_flat,objectsWithBlocks)

  # only tag objects for which " #' @auto_comment " can produce   
  # comments that the rd roclet can transfer to cran-proof documentation .
  # The list of supported classes is intended to grow but we are cautious for now... 
  untagged_objects<-purrr::keep(
    untagged_objects,
    .p=function(obj){
      cls<-c('s4method','s4generic')
      any(vapply(
        cls,
        function(cl){inherits(obj,cl)},
        FUN.VALUE=logical(1)
      ))
    }
  )
  untagged_objects
}




#' @export
roclet_output.roclet_auto_comment<- function(x, results, base_path, ...) {
  # first handle the untagged objects
  # first sort all the undocumented objects by the file
  # they are defined in
  files <- unique(
    lapply(
      results,
      function(o){
        utils::getSrcFilename(attr(o,"srcref"))
      }
    )
  )
  for (file in files){
    p<-file.path(base_path,"R",file)
    lines<-read_lines(p)
    # find the untagged objects in a specific file
    os<-results[
      as.logical(
        lapply(
          results,
          function(o){
            utils::getSrcFilename(attr(o,"srcref"))==file
          }
        )
      )
    ]
    # now sort the undocumented objects in the order in which they appear in the file
    locations_org<-unlist(lapply(
      os,
      function(o){utils::getSrcLocation(attr(o,"srcref"))}
    ))
    
    ord_locations<-locations_org[order(locations_org)]
    # now also order the objects
    ord_os<-os[order(locations_org)]
    boundaries<-c(ord_locations,length(lines)+1)
    extendedChunks<-lapply(
      1:length(ord_locations), 
      function(i){
        start_line<-boundaries[[i]]
        end_line<-boundaries[[i+1]]-1
        chunk<-lines[start_line:end_line]
        o<-ord_os[[i]]
        extended_chunk<-c(
          object_to_block_lines(o),
          chunk
        )
        extended_chunk
      }
    )
    # It is possible that some non roxygen related comments exist before the
    # first object creating call
    first<-ord_locations[[1]]
    if(first>1){
      extendedLines<- c(lines[1:first-1],unlist(extendedChunks))
    }else{
      extendedLines<- unlist(extendedChunks)
    }
    write_lines(extendedLines,p)
  }
}
