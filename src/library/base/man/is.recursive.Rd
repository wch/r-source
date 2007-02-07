\name{is.recursive}
\alias{is.atomic}
\alias{is.recursive}
\title{Is an Object Atomic or Recursive?}
\usage{
is.atomic(x)
is.recursive(x)
}
\description{
  \code{is.atomic} returns \code{TRUE} if \code{x} is an atomic vector
  (or \code{NULL}) and \code{FALSE} otherwise.

  \code{is.recursive} returns \code{TRUE} if \code{x} has a recursive
  (list-like) structure and \code{FALSE} otherwise.
}
\arguments{
  \item{x}{object to be tested.}
}
\details{
  \code{is.atomic} is true for the atomic vector types
  (\code{"logical"}, \code{"integer"}, \code{"numeric"},
  \code{"complex"}, \code{"character"} and \code{"raw"}) and \code{NULL}.

  Most types of language objects are regarded as recursive: those which
  are not are the atomic vector types, \code{NULL} and symbols (as given
  by \code{\link{as.name}}).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{is.list}},
  \code{\link{is.language}}, etc,
  and the \code{demo("is.things")}.
}
\examples{
is.a.r <- function(x) c(is.atomic(x), is.recursive(x))

is.a.r(c(a=1,b=3))      # TRUE FALSE
is.a.r(list())          # FALSE TRUE ??
is.a.r(list(2))         # FALSE TRUE
is.a.r(lm)              # FALSE TRUE
is.a.r(y ~ x)           # FALSE TRUE
is.a.r(expression(x+1)) # FALSE TRUE (not in 0.62.3!)
}
\keyword{programming}
\keyword{classes}
