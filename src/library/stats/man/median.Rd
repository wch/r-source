\name{median}
\title{Median Value}
\usage{
median(x, na.rm = FALSE)
}
\alias{median}
\arguments{
  \item{x}{a numeric vector containing the values
    whose median is to be computed.}
  \item{na.rm}{a logical value indicating whether \code{NA}
    values should be stripped before the computation proceeds.}
  }
\description{
  Compute the sample median of the vector of values given as its argument.
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
