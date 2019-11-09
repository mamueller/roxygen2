
# here a comment for a2 is missing 
# to check if it will be added by the update_autocomment_roclet

#' automatice title
#' 
#' @param a1 see method arguments
#' @s4methods
#' @autocomment 
setGeneric(
    name="G1",
    def=function (a1,a2){
        standardGeneric("G1")
    }
)


# here the documented parameter a3 is no longer part of 
# the code and should be removed

#' automatic title
#' 
#' @param a1 no manual documentation
#' @param a2 no manual documentation
#' @param a3 the superflous param
#' with more than one line of description
#' @autocomment 
setMethod(f="G1",
  signature=signature(
    a1="numeric"
  ),
  definition=function(a1,a2){
     return()
  }
)

