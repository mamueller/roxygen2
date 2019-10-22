autotag_roclet <- function() {
  roclet("autotag")
}

#' @export
roclet_process.roclet_autotag <- function(x, blocks, env, base_path) {
  # The list of blocks that this function receives as an argument is incomplete
  # since some of the objects have not been tagged yet but are going to be
  # tagged automatically with "#' @auto" by this very roclet.
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
              # create a pseudo block to be able to call object_from_call
              b<-roxy_block(tags=list(),file=file,line=5000L,call=call,object=NULL)
              
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

  a_o_flat=list()
  for(o in a_o) {a_o_flat<-append(a_o_flat,o)}

  objectsWithBlocks<-lapply(blocks,function(block){block$object}) 
  # find the undocumented objects
  results<-setdiff(a_o_flat,objectsWithBlocks)

  # only tag objects for which " #' @auto " can produce a cran-proof documentation 
  # The list of supported classes is intended to grow but we are cautious for now... 
  results<-purrr::keep(
    results,
    .p=function(obj){
      cls<-c('s4method','s4generic')
      any(vapply(
        cls,
        function(cl){inherits(obj,cl)},
        FUN.VALUE=logical(1)
      ))
    }
  )
  results
}

#' @export
roclet_output.roclet_autotag <- function(x, results, base_path, ...) {
  # first sort all the undocumented objects by the file
  # they are defined in
  files=unique(
    lapply(
      results,
      function(o){
        utils::getSrcFilename(attr(o,"srcref"))
      }
    )
  )
  for (file in files){
    p<-file.path(base_path,"R",file)
    p2<-file.path(base_path,"R",paste0(file,".new"))
    lines<-read_lines(p)
    # find the untagged objects in that file
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
    boundaries<-c(ord_locations,length(lines)+1)
    chunks<-lapply(
      1:length(ord_locations), # It is possible that some non roxygen related comments exist before the first call
      function(i){
        start_line<-boundaries[[i]]
        end_line<-boundaries[[i+1]]-1
        chunk<-lines[start_line:end_line]
      }
    )
    extendedChunks<-lapply(
      chunks,
      function(chunk){
        extended_chunk<-c("#' @auto",chunk)
      }
    )
    first<-ord_locations[[1]]
    if(first>1){
      extendedLines<- c(lines[1:first-1],unlist(extendedChunks))
    }else{
      extendedLines<- unlist(extendedChunks)
    }
    write_lines(extendedLines,p)
  }

  #NAMESPACE <- file.path(base_path, "NAMESPACE")
  #results <- c(made_by("#"), results)

  ## Always check for roxygen2 header before overwriting NAMESPACE (#436),
  ## even when running for the first time
  #write_if_different(NAMESPACE, results, check = TRUE)

  #NAMESPACE
}
