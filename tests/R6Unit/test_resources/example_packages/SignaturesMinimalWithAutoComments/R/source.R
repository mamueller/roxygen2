#' automatic title
#' 
#' @param a1 see method arguments
#' @param a2 see method arguments
#' @s4methods
#' @autocomment, These comments were created by the auto_comment_roclet by
#' inspection of the package. If you want to change these comments and avoid
#' your changes to be overwritten remove the @auto_comment tag!
setGeneric(
    name="G1",
    def=function (a1,a2){
        standardGeneric("G1")
    }
)

#' automatic title
#' 
#' @param a1 ,object of class:\code{numeric}, found automatically by
#' inspection, not documented yet.
#' @param a2 ,found automatically by inspection, not documented yet.
#' @autocomment, These comments were created by the auto_comment_roclet by
#' inspection of the package. If you want to change these comments and avoid
#' your changes to be overwritten remove the @autocomment tag!
setMethod(f="G1",
  signature=signature(
    # some comments not intended for the docs
    a1="numeric"
  ),
  definition=function(a1,a2){
     return()
  }
)

#' one (partly) documented method
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
