\name{as}
\alias{as}
\alias{as<-}
\alias{coerce}
\title{Force an Object to Belong to a Class}
\usage{
as(object, Class, coerceFlag=T)

coerce(x, y)

}
\description{
  
  \code{as}: 
  Returns the version of this object coerced to be the given \code{Class}.  
  
  \code{coerce}: 
  Coerce \code{x} to be of the same class as \code{y}.  
}
\details{
  
  \code{as}: 
  
If the corresponding \code{is} relation is true, it will be used.  In particular,
if the relation has a coerce method, the method will be invoked on \code{object}.

If the \code{is} relation is FALSE, and \code{coerceFlag} is \code{TRUE},
the coerce function will be called (which will throw an error if there is
no valid way to coerce the two objects).  Otherwise, \code{NULL} is returned.

  
  \code{coerce}: 
  Not a function you should usually
call explicitly.

}
\references{
The web page \url{http://www.omegahat.org/RSMethods/index.html} is the primary documentation.

The functions in this package emulate the facility for classes and methods described in
\emph{Programming with Data}, (John M. Chambers, Springer, 1998).  See this book
for further details and examples.
}
\author{
  John Chambers
}
\keyword{programming}
\keyword{classes}
\keyword{methods}
