\name{weighted.mean}
\title{Weighted Arithmetic Mean}
\usage{
weighted.mean(x, w, na.rm = FALSE)
}
\alias{weighted.mean}
\arguments{
  \item{x}{a numeric vector containing the values
    whose mean is to be computed.}
  \item{w}{a vector of weights the same length as \code{x} giving
    the weights to use for each element of \code{x}.}
  \item{na.rm}{a logical value indicating whether \code{NA}
    values in \code{x} should be stripped before the computation proceeds.}
}
\description{
  Compute a weighted mean of a numeric vector.
}
\details{
  If \code{w} is missing then all elements of \code{x} are
  given the same weight.
  
  Missing values in \code{w} are not handled.
}
\seealso{\code{\link{mean}}
}
\examples{
## GPA from Siegel 1994
wt <- c(5,  5,  4,  1)/15
x <- c(3.7,3.3,3.5,2.8)
xm <- weighted.mean(x,wt)
}
\keyword{univar}
