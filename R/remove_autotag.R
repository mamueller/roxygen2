remove_autotag_roclet <- function() {
  roclet("remove_autotag")
}
#' @export
roclet_process.roclet_remove_autotag <- function(x, blocks, env, base_path) {
  browser()
  
 
  results<-purrr::keep(
    blocks,
    function(block) 'auto' %in% block_tags(block)
  )
  results
}

#' @export
roclet_output.roclet_remove_autotag <- function(x, blocks_with_auto, base_path, ...) {
  # first sort all the undocumented objects by the file
  # they are defined in
  files<-unique(lapply(blocks_with_auto,function(block) block$file))

  for (file in files){
    lines<-read_lines(file)
    # find the blocks 
    bs<-blocks_with_auto[
      as.logical(
        lapply(
          blocks_with_auto,
          function(block){
            block$file==file
          }
        )
      )
    ]
    # now sort the undocumented objects in the order in which they appear in the file
    auto_lines<-lapply(
      bs,
      function(block){
        auto_tags<-purrr::keep(
          block$tags,
          .p=function(tag){tag$tag=='auto'}
        )
        auto_tags[[1]]$line
      }
    )
    browser()
    auto_lines
  }
}
