
#' @auto
setGeneric(
    name="G1",
    def=function (a1,a2){
        standardGeneric("G1")
    }
)

#' @auto
setMethod(f="G1",
  signature=signature(
    a1="numeric"
  ),
  definition=function(a1,a2){
     return()
  }
)

#' G1 
#'
#' @param a1 The only parameter described
setMethod(f="G1",
  signature=signature(
    a1="numeric",
    a2="character"
  ),
  definition=function (a1,a2) {
     return()
  }
)
