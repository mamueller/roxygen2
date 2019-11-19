#' @auto
setClass(
   Class="Person",
   slots=c(name="character")
)

#' @auto
setClass(
   Class="Customer",
   contains=c("Person"),
   slots=c(nr="numeric",orders="numeric")
)

#' @auto
setClass(
   Class="Employe",
   contains=c("Person"),
   slots=c(sallary="numeric")
)

#' @auto
setClass(
   Class="EmployedCustomer",
   contains=c("Employe","Customer"),
   slots=c(discounts="character")
)

setGeneric(
    name='getName',
    def=function(obj){
      standardGeneric('getName')
    }
)
setMethod(
    f='getName',
    signature=signature(obj="Person"),
    definition=function(obj){
      obj@name
    }
)
setGeneric(
    name='getSallary',
    def=function(obj){
      standardGeneric('getSallary')
    }
)
setMethod(
    f='getSallary',
    signature=signature(obj="Employe"),
    definition=function(obj){
      obj@sallary
    }
)
