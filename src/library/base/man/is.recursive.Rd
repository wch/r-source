% File src/library/base/man/is.recursive.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{is.recursive}
\alias{is.atomic}
\alias{is.recursive}
\title{Is an Object Atomic or Recursive?}
\usage{
is.atomic(x)
is.recursive(x)
}
\description{
  \code{is.atomic} returns \code{TRUE} if \code{x} is of an atomic type
  (or \code{NULL}) and \code{FALSE} otherwise.

  \code{is.recursive} returns \code{TRUE} if \code{x} has a recursive
  (list-like) structure and \code{FALSE} otherwise.
}
\arguments{
  \item{x}{object to be tested.}
}
\details{
  \code{is.atomic} is true for the atomic types
  (\code{"logical"}, \code{"integer"}, \code{"numeric"},
  \code{"complex"}, \code{"character"} and \code{"raw"}) and \code{NULL}.

  Most types of objects are regarded as recursive, except for the atomic
  types, \code{NULL} and symbols (as given by \code{\link{as.name}}).
  
  It is common to call the atomic types \sQuote{atomic vectors}, but
  note that \code{\link{is.vector}} imposes further restrictions: an
  object can be atomic but not a vector (in that sense).

  These are \link{primitive} functions.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{is.list}},
  \code{\link{is.language}}, etc,
  and the \code{demo("is.things")}.
}
\examples{
require(stats)

is.a.r <- function(x) c(is.atomic(x), is.recursive(x))

is.a.r(c(a = 1, b = 3)) # TRUE FALSE
is.a.r(list())          # FALSE TRUE - a list is a list
is.a.r(list(2))         # FALSE TRUE
is.a.r(lm)              # FALSE TRUE
is.a.r(y ~ x)           # FALSE TRUE
is.a.r(expression(x+1)) # FALSE TRUE (nowadays)
}
\keyword{programming}
\keyword{classes}
