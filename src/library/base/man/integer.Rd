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
\details{
  Integer vectors exist so that data can be passed to C or Fortran code
  which expects them, and so that small integer data can be represented
  exactly and compactly.

  Note that on almost all implementations of \R the range of
  representable integers is restricted to about
  \eqn{\pm 2 \times 10^9}{+/-2*10^9}: \code{\link{double}}s can hold
  much larger integers exactly.
}
\value{
  \code{integer} creates a integer vector of the specified length.
  Each element of the vector is equal to \code{0}.

  \code{as.integer} attempts to coerce its argument to be of integer
  type. The answer will be \code{NA} unless the coercion succeeds.
  Real values larger in modulus than the largest integer are coerced to
  \code{NA} (unlike S which gives the most extreme integer of the same sign).
  Non-integral numeric values are truncated towards zero (i.e.,
  \code{as.integer(x)} equals \code{\link{trunc}(x)} there), and
  imaginary parts of complex numbers are discarded (with a warning).
  Character strings containing either a decimal representation or a
  hexadecimal representation (starting with \code{0x} or \code{0X}) can
  be converted, as well as any allowed by the platform for real numbers.
  Like \code{\link{as.vector}} it strips attributes including names.

  \code{is.integer} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of integer \link{type} or not, unless it is a
  factor when it returns \code{FALSE}.
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
