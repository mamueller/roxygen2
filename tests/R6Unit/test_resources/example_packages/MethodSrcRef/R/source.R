#
# vim:set ff=unix expandtab ts=2 sw=2:
# The example reproduces a failure of utils::getSrcref 
# to find the source code of a method.

setGeneric(
	 name="BoundFc",
	 def=function ( format, ...){ standardGeneric("BoundFc") }
 )
#------------------------ Constructors ---------------------------------
#the source position of this method is found correctly
setMethod(
  f="BoundFc",
  signature=signature(format='character'),
  definition=function(format, ...){

    a <- 1+1

    return(a) 
  }
)
#---------------------------------------------------------------------------------------------------------
#the source position of this method is not found but yields a list
setMethod(
  f="BoundFc",
  signature=signature(format='missing'),
  definition=function
  (...
 ){
    l <- list(...)
    a <- 1+1

    return(a) 
  }
)
# This is a workaround for the second method
# we include the format argument back in the 'definition' 
# although it will never be present (it is 'missing' in the signature)
# This is horribly counterintuitive code but it is 
# also what methods::method.skeleton writes after you define the generic
# method.skeleton("BoundFc",signature=c(format='missing'),file=someFile.R)
# uncomment to see the effect
#setMethod(
#  f="BoundFc",
#  signature=signature(format='missing'),
#  definition=function
#  (format,
#   ...
# ){
#    l <- list(...)
#    a <- 1+1
#
#    return(a) 
#  }
#)
#meths <- findMethods('BoundFc')
#m1 <- meths[[1]]
#m2 <- meths[[2]]
#getSrcLocation(m1) #ok
#getSrcLocation(m2) # this one yields a list if one takes the uncommented definition and works ok if one uses the workaround
#
## at the root of the problem is methods::unRematchDefinition
## which applied to m2 yields basically the workaround version
#getSrcref(unclass(unRematchDefinition(m2)))
#isRematched(m2)
