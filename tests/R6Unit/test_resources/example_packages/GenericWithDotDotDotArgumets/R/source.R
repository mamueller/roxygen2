setGeneric(
    name="G1",
    def=function # a function
    ### A gneric funcion 
    ### note that we do not document the arguments here
    ### since we let inlinedocs add a hint to the methods here
    (a1,...){
        standardGeneric("G1")
    }
)

setMethod(f="G1",
  signature=signature(
    a1="numeric"
  ),
  definition=function # a method for G1  
  ### description
  ## All methods have to implement the ... argument although it is not in the signature
  (
   a1, ##<< a variable
   ... ##<< blissfully ignored by this function
   )
  {
     return()
  }
)
#setMethod(f="G1",
#  signature=signature(
#    a1="numeric",
#    a2="character"
#  ),
#  definition=function # a method that uses both arguments
#  ### description
#  (a1,a2)
#  {
#     return()
#  }
#)
