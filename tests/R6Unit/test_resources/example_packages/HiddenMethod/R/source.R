#
# vim:set ff=unix expandtab ts=2 sw=2:
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
### Since this class is exported in the Namespace file you can inherit from it
### but nethertheless the method for "hiddenGeneric" with this class as
### a signature will not be visible
setClass(# an Exposed  class
   Class="ExposedClass",
   representation=representation(
        times="numeric"
   )
)
#------------------------------------------------
### Since this class is not exported in the Namespace file you can not inherit from it
setClass(# a hiddeb class
   Class="HiddenClass",
   representation=representation(
        times="numeric"
   )
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
