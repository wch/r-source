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
\arguments{
    \item{arglist}{Empty or one or more name or name=expression terms.}
    \item{value}{An expression, or a series of expressions separated
        by commas.}
}
\details{
    In \R (unlike S) the names in an argument list cannot be quoted
    non-standard names.

    If \code{value} is a series of expressions, the value returned is a
    list of the evaluated expressions, with names set to the expressions
    where these are the names of \R objects.
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
