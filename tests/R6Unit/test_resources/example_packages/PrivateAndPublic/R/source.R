
setClass(# an private class
   Class="VirtualParentParentClass",
   contains='VIRTUAL',
)
setClass(# an Exposed  class
   Class="VirtualParentClass",
   contains=c('VirtualParentParentClass','VIRTUAL')
)
setClass(# a private class
   Class="OtherParentClass",
   slots=c(times="numeric")
)

setClass(# an Exposed  class
   Class="RealClass",
   contains=c('VirtualParentClass','OtherParentClass'),
   slots=c(times="numeric")
)

