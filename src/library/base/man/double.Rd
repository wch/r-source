\name{double}
\title{Double-Precision Vectors}
\alias{double}
\alias{as.double}
\alias{as.double.default}
\alias{is.double}
\alias{single}
\alias{as.single}
\alias{as.single.default}
\description{
  Create, coerce to or test for a double-precision vector.
}
\usage{
double(length = 0)
as.double(x, \dots)
is.double(x)

single(length = 0)
as.single(x, \dots)
}
\arguments{
  \item{length}{desired length.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{double} creates a double-precision vector of the specified
  length.  The elements of the vector are all equal to \code{0}.
  It is identical to \code{\link{numeric}} (and \code{real}).

  \code{as.double} is a generic function.  It is identical to
  \code{as.numeric} (and \code{as.real}).

  \code{is.double} is a test of double \link{type}.
  
  \emph{\R has no single precision data type.  All real numbers are
    stored in double precision format}.  The functions \code{as.single}
  and \code{single} are identical to \code{as.double} and \code{double}
  except they set the attribute \code{Csingle} that is used in the
  \code{\link{.C}} and \code{\link{.Fortran}} interface, and they are
  intended only to be used in that context.
}
\value{
  \code{double} creates a double-precision vector of the specified
  length.  The elements of the vector are all equal to \code{0}.

  \code{as.double} attempts to coerce its argument to be of double
  type: like \code{\link{as.vector}} it strips attributes including
  names.  (To ensure that an object is of double type without
  stripping attributes, use \code{\link{storage.mode}}.)
  Character strings containing either a decimal representation or a
  hexadecimal representation (starting with \code{0x} or \code{0X}) can
  be converted. \code{as.double} for factors yields the codes underlying
  the factor levels, not the numeric representation of the labels, see
  also \code{\link{factor}}.

  \code{is.double} returns \code{TRUE} or \code{FALSE} depending on
  whether its argument is of double \link{type} or not.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{integer}}, \code{\link{numeric}}, \code{\link{storage.mode}}.
}
\examples{
is.double(1)
all(double(3) == 0)
}
\keyword{classes}
