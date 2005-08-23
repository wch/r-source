\name{ARMAacf}
\alias{ARMAacf}
\title{Compute Theoretical ACF for an ARMA Process}
\description{
 Compute the theoretical autocorrelation function or partial
 autocorrelation function for an ARMA process.
}
\usage{
ARMAacf(ar = numeric(0), ma = numeric(0), lag.max = r, pacf = FALSE)
}
\arguments{
  \item{ar}{numeric vector of AR coefficients}
  \item{ma}{numeric vector of MA coefficients}
  \item{lag.max}{integer.  Maximum lag required.  Defaults to
    \code{max(p, q+1)}, where \code{p, q} are the numbers of AR and MA
    terms respectively.}
  \item{pacf}{logical.  Should the partial autocorrelations be returned?}
}
\details{
  The methods used follow Brockwell \& Davis (1991, section 3.3).  Their
  equations (3.3.8) are solved for the autocovariances at lags
  \eqn{0, \dots, \max(p, q+1)}, and the remaining autocorrelations are
  given by a recursive filter.
}
\value{
  A vector of (partial) autocorrelations, named by the lags.
}

\references{
  Brockwell, P. J. and Davis, R. A. (1991) \emph{Time Series: Theory and
    Methods}, Second Edition.  Springer.
}
\seealso{\code{\link{arima}}, \code{\link{ARMAtoMA}}, \code{\link{filter}}.}

\examples{
ARMAacf(c(1.0, -0.25), 1.0, lag.max = 10)

## Example from Brockwell & Davis (1991, pp.92-4)
## answer 2^(-n) * (32/3 + 8 * n) /(32/3)
n <- 1:10; 2^(-n) * (32/3 + 8 * n) /(32/3)
ARMAacf(c(1.0, -0.25), 1.0, lag.max = 10, pacf = TRUE)
ARMAacf(c(1.0, -0.25), lag.max = 10, pacf = TRUE)

## Cov-Matrix of length-7 sub-sample of AR(1) example:
toeplitz(ARMAacf(0.8, lag.max = 7))
}
\keyword{ts}
