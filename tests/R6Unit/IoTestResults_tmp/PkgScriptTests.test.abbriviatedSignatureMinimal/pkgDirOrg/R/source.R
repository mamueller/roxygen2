setGeneric(
    name="G1",
    def=function # a function
    ### A gneric funcion 
    ### note that we do not document the arguments here
    ### since we let inlinedocs add a hint to the methods here
    (a1,a2){
        standardGeneric("G1")
    }
)

setMethod(f="G1",
  signature=signature(
    a1="numeric"
  ),
  definition=function # a method that uses only one argument
  ### description
  (a1)
  {
     return()
  }
)
setMethod(f="G1",
  signature=signature(
    a1="numeric",
    a2="character"
  ),
  definition=function # a method that uses both arguments
  (a1,a2)
  {
     return()
  }
)
