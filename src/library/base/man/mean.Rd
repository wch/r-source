\name{mean}
\title{Arithmetic Mean}
\usage{
mean(x, \dots)

\method{mean}{default}(x, trim = 0, na.rm = FALSE, \dots)
}
\alias{mean}
\alias{mean.default}
\alias{mean.data.frame}
\arguments{
  \item{x}{An \R object.  Currently there are methods for numeric data
    frames, numeric vectors and dates.  A complex vector is allowed for
    \code{trim = 0}, only.}
  \item{trim}{the fraction (0 to 0.5) of observations to be
    trimmed from each end of \code{x} before the mean is computed.}
  \item{na.rm}{a logical value indicating whether \code{NA}
    values should be stripped before the computation proceeds.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  Generic function for the (trimmed) arithmetic mean.
}
\value{
  For a data frame, a named vector with the appropriate method being
  applied column by column.
  
  If \code{trim} is zero (the default),
  the arithmetic mean of the values in \code{x} is computed.

  If \code{trim} is non-zero, a symmetrically trimmed mean is computed
  with a fraction of \code{trim} observations deleted from each end
  before the mean is computed.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{weighted.mean}}, \code{\link{mean.POSIXct}}
}
\examples{
x <- c(0:10, 50)
xm <- mean(x)
c(xm, mean(x, trim = 0.10))

mean(USArrests, trim = 0.2)
}
\keyword{univar}

