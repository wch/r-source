\name{function}
\title{Function Definition}
\usage{
function( arglist ) expr
return(value)
}
\alias{function}
\alias{return}
\description{
These functions provide the base mechanisms for defining
new functions in the \R language.
}
\seealso{
  \code{\link{args}} and \code{\link{body}} for accessing the arguments
  and body of a function.
  \code{\link{debug}} for debugging; \code{\link{invisible}} for
  \code{return(.)}ing \emph{invisibly}.
}
\examples{
norm <- function(x) sqrt(x\%*\%x)
norm(1:4)

## An anonymous function:
(function(x,y){ z <- x^2 + y^2; x+y+z })(0:7, 1)
}
\keyword{programming}
