\name{double}
\title{Double Precision Vectors}
\alias{double}
\alias{as.double}
\alias{as.double.default}
\alias{is.double}
\alias{single}
\alias{as.single}
\alias{as.single.default}
\usage{
double(length = 0)
as.double(x)
is.double(x)
single(length = 0)
as.single(x)
}
\description{
    Create, coerce to or test for a double-precision vector.
}
\value{
  \code{double} creates a double precision vector of the specified
  length.  The elements of the vector are all equal to \code{0}.

  \code{as.double} attempts to coerce its argument to be of double
  type.

  \code{is.double} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of double type or not.
}
\note{
  \emph{\R has no single precision data type.  All real numbers are
    stored in double precision format}.  The functions \code{as.single}
  and \code{single} are identical to \code{as.double} and \code{double}
  except they set the attribute \code{Csingle} that is used in the
  \code{\link{.C}} and \code{\link{.Fortran}} interface, and they are
  intended only to be used in that context.
}
\seealso{
  \code{\link{integer}}.
}
\examples{
is.double(1)
all(double(3) == 0)
}
\keyword{classes}
