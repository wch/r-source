\name{body}
\alias{body}
\alias{body<-}
\title{Access to and Manipulation of the Body of a Function}
\description{
  Get or set the body of a function.
}
\usage{
body(fun = sys.function(sys.parent()))
body(fun, envir = parent.frame()) <- value
}
\arguments{
  \item{fun}{a function object, or see Details.}
  \item{envir}{environment in which the function should be defined.}
  \item{value}{an expression or a list of \R expressions.}
}
\value{
  \code{body} returns the body of the function specified.

  The assignment form sets the body of a function to the list on the
  right hand side.
}
\details{
  For the first form, \code{fun} can be a character string
  naming the function to be manipulated, which is searched for from the
  parent environment.
  If it is not specified, the function calling \code{body} is used.
}
\note{
  For ancient historical reasons, \code{envir = NULL} uses the global
  environment rather than the base environment.  
  Please use \code{envir = \link{globalenv}()} instead if this is what you want,
  as the special handling of \code{NULL} may change in a future release.
}
\seealso{
  \code{\link{alist}},
  \code{\link{args}},
  \code{\link{function}}.
}
\examples{
body(body)
f <- function(x) x^5
body(f) <- expression(5^x)
## or equivalently  body(f) <- list(quote(5^x))
f(3) # = 125
body(f)
}
\keyword{programming}
