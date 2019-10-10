#
# vim:set ff=unix expandtab ts=2 sw=2:

setClass(
  Class="Model",
  slots=list(
    mat="numeric"
  )
)
#---------------------------------------------------------------
setMethod(### documentation is required although the method is not explicitly exported
  f="[",
  signature(x="Model",i="character",j="missing",drop="missing"),
  definition=function# fake [ method
  ### We fake the behavior of the [] operator to provide a more
  ### R like interface.
  (
  x, ##<< the soil model
  i  ##<< the name of the method
  ){
  2 ##<< the result of the get[[PropertyName]] method
  }
)

