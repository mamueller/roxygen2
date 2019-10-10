
### Since this class is exported in the Namespace file you can inherit from it
setClass(# an Exposed  class
   Class="VirtualParentParentClass",
   contains='VIRTUAL',
)
### Since this class is exported in the Namespace file you can inherit from it
setClass(# an Exposed  class
   Class="VirtualParentClass",
   contains=c('VirtualParentParentClass','VIRTUAL')
)
### Since this class is exported in the Namespace file you can inherit from it
setClass(# an Exposed  class
   Class="OtherParentClass",
   slots=c(times="numeric")
)

### This is a virtual class and it will be detected automatically
setClass(# an Exposed  class
   Class="RealClass",
   contains=c('VirtualParentClass','OtherParentClass'),
   slots=c(times="numeric")
)

