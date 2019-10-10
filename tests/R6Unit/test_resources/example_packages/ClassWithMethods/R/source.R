#
# vim:set ff=unix expandtab ts=2 sw=2:
#------------------------------------------------
### a preceding line of comment
simpleFunc <- function(# a title
  ### some more comments
  x1 ##<< a number
  )
  {
  ##<< aditional description
  z <- x1^2 
  z
  ### the result
  }
#------------------------------------------------
setGeneric(
    name="hiddenGeneric",
    def=function( # convert its argument to a Delta14C representation
    ### Thfunction returns an object of the same type as its imput
    ### this can be a number a matrix or an object of class FcAtm
    object ##<< an object that contains data and a formatdescription.  So it can be converted into the AbsoluteFractionModern format if a conversion is implemented.
    ){
        standardGeneric("hiddenGeneric")
    }
)
#------------------------------------------------
### a preceding line of comment
g<-function( # This is the title 
    ### This is the first line of the description
    ### which can go on for several more
      object     ##<< an object that contains data and a formatdescription.  So it can be converted into the AbsoluteFractionModern format if a conversion is implemented.
      ,
      somethingElse ##<< another object
    ){
        standardGeneric("exposedGeneric")
    }

setGeneric(
    name="exposedGeneric",
    def=g
)

### Since this class is exported in the Namespace file you can inherit from it
### but nethertheless the method for "hiddenGeneric" with this class as
### a signature will not be visible
setClass(# an Exposed  class
   Class="ExposedClass",
   slots=c(times="numeric")
)
#------------------------------------------------
# the next method should not appear in the help 
# because the generic function is not exported
setMethod(
   f= "hiddenGeneric",
   signature="ExposedClass",
   definition=function#short title
   ### short description
   (object){
       return(object@times)
     }
)
#------------------------------------------------
### This method should appear in the help 
### because the generic function is exported
setMethod(
   f= "exposedGeneric",
   signature=c("ExposedClass","numeric"),
   ### Preceding lines of comment:
   definition=function#short title
   ### short description
   ##details<< here come a few details
   ##in two lines
   ##examples<<
   ## eci <- new(Class="ExposedClass",times=1:4)
   ## exposedGeneric(eci,3)
   (
     object ##<< an object 
     ,
     somethingElse ##<< an object 
   ){
       return(object@times)
       ### the result
     }
)
#------------------------------------------------
### This method should appear in the help 
### because the generic function is exported
setMethod(
   f= "exposedGeneric",
   signature=c("character","numeric"),
   ### Preceding lines of comment:
   definition=function #short title
   ### short description
   (
     object ##<< an object 
     ,
     somethingElse ##<< an object 
   ){
       return(object@times)
       ### the result
     }
)
#---------------------------------------------------------------
setMethod(### for [ (and others [[,$...) documentation is required although the method is not explicitly exported
  f="[",
  signature(x="ExposedClass",i="character",j="missing",drop="missing"),
  definition=function# fake [ method
  ### We fake the behavior of the [] operator to provide a more
  ### R like interface.
  (
  x, ##<< the soil model
  i  ##<< the name of the method
  ){
    ##details<<
    ## here come the details
    ##anotherFunnySection<<
    ## whatever you want
  2 ##<< the result of the get[[PropertyName]] method
  }
)

