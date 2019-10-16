autotag_roclet <- function() {
  roclet("autotag")
}
#roclet_preprocess.roclet_autotag <- function(x, blocks, base_path) {
#
#  invisible(x)
#}

#' @export
roclet_process.roclet_autotag <- function(x, blocks, env, base_path) {
  # The list of blocks that this function receives as an argument is incomplete
  # since some of the objects have not been tagged yet but are going to be
  # tagged automatically with @auto by this very roclet.
  # we therefore parse the whole package using Rs own parse function
  env <- env_package(base_path)
  files <- package_files(base_path)
  a_o<- lapply(
    files,
    function(file){
      print(file)
      sf<-srcfile(file)
      exprs<-parse(file,keep.source=TRUE,srcfile=sf)
      browser()
      print(exprs)
      objects_from_file<-lapply(
        1:length(exprs),
        function(i){
          print(i)
          call<-exprs[[i]]
          o<-object_from_call( call=call, env=env, file=file)
          attr(o,"srcref")<-attr(exprs,'srcref')[[i]]
          o
        }
      )
    }
   )

  a_o_flat=list()
  for(o in a_o) {a_o_flat<-append(a_o_flat,o)}

  objectsWithBlocks<-lapply(blocks,function(block){block$object}) 
  # find the undocumented objects
  results<-setdiff(a_o_flat,objectsWithBlocks)
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
      results,
      function(o){utils::getSrcLocation(attr(o,"srcref"))}
    ))
    browser()
    
    ord_locations<-locations_org[order(locations_org)]
    boundaries<-c(0,ord_locations,length(lines))
    extendedLines<-unlist(lapply(
      2:length(ord_locations), # It is possible that some non roxygen related comments exist before the first call
      function(i){
        start_line<-boundaries[[i]]
        end_line<-boundaries[[i+1]]
        chunk<-lines[start_line:end_line]
        extended_chunk<-c("#' @auto",chunk)
        extended_chunk
      }
    ))
    write_lines(extendedLines,p2)


  }

  #NAMESPACE <- file.path(base_path, "NAMESPACE")
  #results <- c(made_by("#"), results)

  ## Always check for roxygen2 header before overwriting NAMESPACE (#436),
  ## even when running for the first time
  #write_if_different(NAMESPACE, results, check = TRUE)

  #NAMESPACE
}

#' @export
roclet_clean.roclet_autotag <- function(x, base_path) {
  #NAMESPACE <- file.path(base_path, "NAMESPACE")
  #if (made_by_roxygen(NAMESPACE)) {
  #  unlink(NAMESPACE)
  #}
}
