\name{integer}
\alias{integer}
\alias{as.integer}
\alias{as.integer.default}
\alias{is.integer}
\title{Integer Vectors}
\description{
  Creates or tests for objects of type \code{"integer"}.
}
\usage{
integer(length = 0)
as.integer(x, \dots)
is.integer(x)
}
\value{
  \code{integer} creates a integer vector of the specified length.
  Each element of the vector is equal to \code{0}.
  Integer vectors exist so that data can be passed to C or Fortran code
  which expects them.

  \code{as.integer} attempts to coerce its argument to be of integer
  type. The answer will be \code{NA} unless the coercion succeeds.
  Real values larger in modulus than the largest integer are coerced to
  \code{NA} (unlike S which gives the most extreme integer of the same sign).

  \code{is.integer} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of integer type or not. 
}
\keyword{classes}
