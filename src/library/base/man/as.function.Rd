\name{as.function}
\title{Convert Object to Function}
\usage{
as.function(x, \dots)

\method{as.function}{default}(x, envir = parent.frame(), \dots)
}
\alias{as.function}
\alias{as.function.default}
\arguments{
  \item{x}{object to convert, a list for the default method.}
  \item{\dots}{additional arguments, depending on object}

  \item{envir}{environment in which the function should be defined}
}
\description{
  \code{as.function} is a generic function which is used to convert
  objects to functions.

  \code{as.function.default} works on a list \code{x}, which should contain the
  concatenation of a formal argument list and an expression or an
  object of mode \code{"\link{call}"} which will become the function body.
  The function will be defined in a specified environment, by default
  that of the caller.
}
\value{
  The desired function.
}
\author{Peter Dalgaard}
\seealso{
  \code{\link{function}};
  \code{\link{alist}} which is handy for the construction of
  argument lists, etc.
}
\examples{
as.function(alist(a=,b=2,a+b))
as.function(alist(a=,b=2,a+b))(3)
}
\keyword{programming}
