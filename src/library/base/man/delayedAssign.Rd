\name{delayedAssign}
\alias{delayedAssign}
\concept{promise}
\concept{promises}
\title{Delay Evaluation}
\description{
\code{delayedAssign} creates a \emph{promise} to evaluate the given
expression if its value is requested.  This provides direct access 
to the \emph{lazy evaluation} mechanism used by \R for the evaluation 
of (interpreted) functions.
}
\usage{
delayedAssign(x, value, eval.env = parent.frame(1), 
	      assign.env = parent.frame(1))
}
\arguments{
  \item{x}{a variable name (given as a quoted string in the function call)}
  \item{value}{an expression to be assigned to \code{x}}
  \item{eval.env}{an environment in which to evaluate \code{value}}
  \item{assign.env}{an environment in which to assign \code{x}}
}
\value{
  This function is invoked for its side effect, which is assigning
  a promise to evaluate \code{value} to the variable \code{x}.  
}
\details{
  Both \code{eval.env} and \code{assign.env} default to the currently active
  environment.

  The expression assigned to a promise by \code{delayedAssign} will
  not be evaluated until it is eventually ``forced''. This happens when
  the variable is first accessed.
  
  When the promise is eventually forced, it is evaluated within the
  environment specified by \code{eval.env} (whose contents may have changed in
  the meantime).  After that, the value is fixed and the expression will
  not be evaluated again.
  
  This function is meant to replace the \code{delay()} function, to make it
  more difficult for R code to see ``naked'' promises.
}
\seealso{
\code{\link{substitute}}, to see the expression associated with a promise.
}
\examples{
msg <- "old"
delayedAssign("x", msg)
msg <- "new!"
x #- new!
substitute(x) #- msg

delayedAssign("x", {
    for(i in 1:3)
        cat("yippee!\n")
    10
})

x^2 #- yippee
x^2 #- simple number

e <- (function(x, y = 1, z) environment())(1+2, "y", {cat(" HO! "); pi+2})
(le <- as.list(e)) # evaluates the promises

}
\keyword{programming}
\keyword{data}
