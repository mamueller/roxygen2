#' build a G
#'
setGeneric(
    name="G1",
    def=function (a1,a2){
        standardGeneric("G1")
    }
)

#' build a G
#'
setMethod(f="G1",
  signature=signature(
    a1="numeric"
  ),
  definition=function(a1){
     return()
  }
)

#' build a G
#'
setMethod(f="G1",
  signature=signature(
    a1="numeric",
    a2="character"
  ),
  definition=function (a1,a2) {
     return()
  }
)
