#
# vim:set ff=unix expandtab ts=2 sw=2:
setGeneric(
  name="G",
    def=function # THIS GOES IN THE TITLE 
    ### Do something in a complete sentence.
    (n,p,r){
        standardGeneric("G")
    }
)
#--------------------------------------------
setMethod(f="G",
  signature=signature(
    n="integer"
  ),
  definition=function#Test an integer for primality with Fermat's little theorem.
	### Fermat's little theorem states that if \eqn{n} is a prime number
	### and \eqn{a} is any positive integer less than \eqn{n}, then
	### \eqn{a} raised to the \eqn{n}th power is congruent to \eqn{a\
	### modulo\ n}{a modulo n}.
	##references<< \url{http://en.wikipedia.org/wiki/Fermat's_little_theorem}
	(
	n ##<< the integer to test for primality.
	 ){
	  a <- floor(runif(1,min=1,max=n))
	  ##note<< \code{fermat.test} doesn't work for integers above
	  ##approximately 15 because modulus loses precision.
	  a^n %% n == a
	### Whether the integer passes the Fermat test for a randomized
	### \eqn{0<a<n}
	}
)

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
  (
  x, ##<< the soil model
  i  ##<< the name of the method
  ){
  2
  }
)

#---------------------------------------------------------------
setMethod(### Although plot is as [ a predefined function documentation is required only if the method is reported just as for any
    f="plot",
    signature=c(x="character",y="character"),
      definition=function# fake plot method 
      ### This method plots all the results computed with the Monte Carlo simulation
      (
      x,y
      )
      { 
        return(3)
      }
)
