\name{median}
\title{Median Value}
\usage{
median(x, na.rm=FALSE)
}
\alias{median}
\description{
\code{median}
computes the sample median of the vector of values given as its argument.
If \code{na.rm} is \code{TRUE} then \code{NA} values are deleted
before computation proceeds.
}
\seealso{\code{\link{quantile}} for general quantiles.}
\examples{
median(1:4)# = 2.5 [even number]
median(c(1:3,100,1000))# = 3 [odd, robust]
}
\keyword{univar}
\keyword{robust}
