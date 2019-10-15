setGeneric(
    name="G1",
    def=function (a1,a2){
        standardGeneric("G1")
    }
)

setMethod(f="G1",
  signature=signature(
    a1="numeric"
  ),
  definition=function(a1,a2){
     return()
  }
)

setMethod(f="G1",
  signature=signature(
    a1="numeric",
    a2="character"
  ),
  definition=function (a1,a2) {
     return()
  }
)
