remove_autotag_roclet <- function() {
  roclet("remove_autotag")
}
#' @export
roclet_process.roclet_remove_autotag <- function(x, blocks, env, base_path) {
  
 
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
  files<-unique(lapply(blocks_with_auto,function(block){block$file}))

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
    # find the @auto lines
    auto_pos<-unlist(
      lapply(
        bs,
        function(block){
          lapply(
            purrr::keep(
              block$tags,
              .p=function(tag){tag$tag=='auto'}
            ),
            function(tag){tag$line}
          )
        }
      )
    )
    auto_pos
    base::writeLines(
      text=lines[setdiff(seq_along(lines),auto_pos)],
      con=file
    )
  }
}
