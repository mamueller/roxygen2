#
# vim:set ff=unix expandtab ts=2 sw=2:
#------------------------------------------------

### Since this class is exported in the Namespace file you can inherit from it
setClass(# an Exposed  class
   Class="ExposedClass",
   slots=c(times="numeric")
   ##exampleFunctionsFromFiles<< 
   ##inst/examples/examples_1.R  func1
   ##inst/examples/examples_1.R  func2
   
   ##examples<<
   ## eci <- new(Class="ExposedClass",times=1:4)
)
   

