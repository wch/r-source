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
\arguments{
  \item{length}{desired length.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
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
  Non-integral numeric values are truncated towards zero (i.e.,
  \code{as.integer(x)} equals \code{\link{trunc}(x)} there), and
  imaginary parts of complex numbers are discarded (with a warning).
  Like \code{\link{as.vector}} it strips attributes including names.

  \code{is.integer} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of integer type or not.
  \code{is.integer} is generic: you can write methods to handle
  of specific classes of objects, see \link{InternalMethods}.
  Note that factors are true for \code{is.integer} but false for
  \code{\link{is.numeric}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{round}} (and \code{ceiling} and \code{floor} on that help
  page) to convert to integral values.
}
\examples{
  ## as.integer() truncates:
  x <- pi * c(-1:1,10)
  as.integer(x)
}
\keyword{classes}
