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
