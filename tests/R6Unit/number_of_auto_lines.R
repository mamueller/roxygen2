number_of_auto_lines<-function(pkgDir){
  pR=file.path(pkgDir,'R')
  src_files<-lapply(list.files(pR),function(file_name){file.path(pR,file_name)})
  code=unlist(lapply(src_files,roxygen2:::read_lines))
  #browser()
  length(grep(pattern="^#' @auto",code))
}
