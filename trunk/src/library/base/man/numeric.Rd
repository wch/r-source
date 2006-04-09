\name{numeric}
\title{Numeric Vectors}
\alias{numeric}
\alias{as.numeric}
\alias{is.numeric}
\alias{is.numeric.factor}
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

  Note that factors are false for \code{is.numeric} since there is a
  \code{"factor"} method.
}
\value{
  \code{numeric} creates a real vector of the specified length.  The
  elements of the vector are all equal to \code{0}.

  \code{as.numeric} attempts to coerce its argument to a numeric \link{type}
  (either integer or real).
  \code{as.numeric} for factors yields the codes underlying the factor
  levels, not the numeric representation of the labels.
    
  \code{is.numeric} returns \code{TRUE} if its argument is of
  \link{mode} \code{"numeric"} (\link{type} \code{"double"} or type
  \code{"integer"}) and not a factor, and \code{FALSE} otherwise.
}

\note{
  \emph{\R has no single precision data type.  All real numbers are
    stored in double precision format}.
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
