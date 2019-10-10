#
# vim:set ff=unix expandtab ts=2 sw=2:
setClass(
  Class="M1",
  slots=list(
    mat="numeric"
  )
)
setMethod(f="[[",
  signature=signature(
    x="M1",
    i="character"
  ),
  definition=function # a method that uses only one argument
  ### description
  (x, ##<< the model to be accessed
   i  ##<< the string to be used in place of the column
  )
  {
    if (i=="mat") {
     return(x@mat)
    }else{
      stop("blub")
    }
  }
)
setMethod(f="[[<-",
  signature=signature(
    x="M1"
  ),
  definition=function # a method that uses only one argument
  ### description
  (x, ##<< the model to be accessed
   i,  ##<< the string to be used in place of the column
   value ##<<the replacement value
  )
  {
    if (i=="mat") {
       x@mat<-value
       return(x)
    }else{
      stop("blub")
    }
   ### examples
   ##<< m<-new("M1")
   ##<< m[["mat"]]<-5
   ##<< m[["mat"]]
  }
)
