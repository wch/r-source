\name{numeric}
\title{Numeric Vectors}
\alias{numeric}
\alias{as.numeric}
\alias{is.numeric}
\alias{is.numeric.Date}
\alias{is.numeric.POSIXt}
\description{
  Creates or coerces objects of type \code{"numeric"}.
  \code{is.numeric} is a more general test of an object being
  interpretable as numbers.
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
  \code{numeric} is identical to \code{\link{double}} (and \code{real}).
  It creates a double-precision vector of the specified length with each
  element equal to \code{0}.

  \code{as.numeric} is a generic function, but S3 methods must be
  written for \code{\link{as.double}}.  It is identical to
  \code{as.double} (and \code{as.real}).

  \code{is.numeric} is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.  It is
  \strong{not} the same as \code{\link{is.double}}.  Factors are handled
  by the default method, and there are methods for classes
  \code{"\link{Date}"} and \code{"\link{POSIXt}"} (in all three cases
  the result is false).  Methods for \code{is.numeric} should only
  return true if the base type of the class is \code{double} or
  \code{integer} \emph{and} and values can reasonably be regarded as
  numeric (e.g. arithmetic on them makes sense).
}
\value{
  for \code{numeric} and \code{as.numeric} see \code{\link{double}}.
  
  The default method for \code{is.numeric} returns \code{TRUE}
  if its argument is of \link{mode} \code{"numeric"}
  (\link{type} \code{"double"} or type \code{"integer"}) and not a
  factor, and \code{FALSE} otherwise.  That is,
  \code{is.integer(x) || is.double(x)}, or
  \code{(mode(x) == "numeric") && !is.factor(x)}.
}
\section{S4 methods}{
  \code{as.numeric} and \code{is.numeric} are internally S4 generic and
  so methods can be set for them \emph{via} \code{setMethod}.

  To ensure that \code{as.numeric}, \code{as.double} and \code{as.real}
  remain identical, S4 methods can only be set for \code{as.numeric}.
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
