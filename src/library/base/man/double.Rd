\name{double}
\title{Double Precision Vectors}
\usage{
double(n = 0)

as.double(x)
is.double(x)
}
\alias{double}
\alias{as.double}
\alias{is.double}
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
    stored in double precision format}.
}
\seealso{
  \code{\link{integer}}.
}
\examples{
is.double(1)
all(double(3) == 0)
}
\keyword{classes}
