\name{numeric}
\title{Numeric Vectors}
\alias{numeric}
\alias{as.numeric}
\alias{is.numeric}
\description{
  Creates or tests for objects of type \code{"numeric"}.
}
\usage{
numeric(length = 0)
as.numeric(x, \dots)
is.numeric(x)
}
\arguments{
  \item{length}{desired length.}
  \item{x}{object to be coerced or tested.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{as.numeric} is a generic function, but methods must be
  written for \code{\link{as.double}}, which it calls.

  \code{is.numeric} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
}
\value{
  \code{numeric} creates a real vector of the specified length.  The
  elements of the vector are all equal to \code{0}.

  \code{as.numeric} attempts to coerce its argument to \code{"double"}
  (even if it is already \code{"integer"}).
  \code{as.numeric} for factors yields the codes underlying the factor
  levels, not the numeric representation of the labels, see also
  \code{\link{factor}}.

  The default method for \code{is.numeric} returns \code{TRUE}
  if its argument is of \link{mode} \code{"numeric"}
  (\link{type} \code{"double"} or type \code{"integer"}) and not a
  factor, and \code{FALSE} otherwise.  That is,
  \code{is.integer(x) || is.double(x)}, or
  \code{(mode(x) == "numeric") && !is.factor(x)}.
}

\note{
  \emph{\R has no single precision data type.  All real numbers are
    stored in double precision format}.

  \code{numeric} and \code{as.numeric} are the identical to \code{double} and
  \code{as.double} (unless someone defines S4 methods on one of them).
}
\seealso{
  \code{\link{double}}, \code{\link{integer}}, \code{\link{storage.mode}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
as.numeric(c("-.1"," 2.7 ","B")) # (-0.1, 2.7, NA)  +  warning
as.numeric(factor(5:10))
}
\keyword{classes}
\keyword{attribute}
