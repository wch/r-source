\name{sd}
\title{Standard Deviation}
\usage{
sd(x, na.rm = FALSE)
}
\alias{sd}
\description{
  This function computes the standard deviation of the values in
  \code{x}.
  If \code{na.rm} is \code{TRUE} then missing values are removed before
  computation proceeds.
}
\seealso{
  \code{\link{var}} for its square, and \code{\link{mad}}, the most
  robust alternative.
}
\examples{
sd(1:2) ^ 2
}
\keyword{univar}
