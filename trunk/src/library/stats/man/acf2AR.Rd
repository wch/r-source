\name{acf2AR}
\alias{acf2AR}
\title{Compute an AR Process Exactly Fitting an ACF}
\description{
  Compute an AR process exactly fitting an autocorrelation function.
}
\usage{
acf2AR(acf)
}
\arguments{
  \item{acf}{An autocorrelation or autocovariance sequence.}
}
\value{
  A matrix, with one row for the computed AR(p) coefficients for
  \code{1 <= p <= length(acf)}.
}
\seealso{
  \code{\link{ARMAacf}}, \code{\link{ar.yw}} which does this from an
  empirical ACF.
}
\examples{
(Acf <- ARMAacf(c(0.6, 0.3, -0.2)))
acf2AR(Acf)
}
\keyword{ts}
