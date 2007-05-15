\name{median}
\title{Median Value}
\alias{median}
\alias{median.default}
\usage{
median(x, na.rm = FALSE)
}
\arguments{
  \item{x}{an object for which a method has been defined, or a
    numeric vector containing the values whose median is to be computed.}
  \item{na.rm}{a logical value indicating whether \code{NA}
    values should be stripped before the computation proceeds.}
}
\description{
  Compute the sample median.
}
\details{
  This is a generic function for which methods can be written.  However,
  the default method makes use of \code{sort} and \code{mean}, both of
  which are generic, and so the default method will work for most
  classes (e.g. \code{"\link{Date}"}) for which a median is a reasonable
  concept.
}
\value{
  The default method returns a length-one object of the same type as
  \code{x}, except when \code{x} is integer of even length, when the
  result will be double.
  
  If there are no values or if \code{na.rm = FALSE} and there are \code{NA}
  values the result is \code{NA} of the same type as \code{x} (or more
  generally the result of \code{x[FALSE][NA]}).
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{\code{\link{quantile}} for general quantiles.}
\examples{
median(1:4)# = 2.5 [even number]
median(c(1:3,100,1000))# = 3 [odd, robust]
}
\keyword{univar}
\keyword{robust}
