\name{tsdiag}
\alias{tsdiag}
\alias{tsdiag.arima0}
\alias{arima0.diag}
\title{Diagnostic Plots for Time-Series Fits}
\description{
  A generic function to plot time-series diagnostics.
}
\usage{
tsdiag(object, gof.lag, ...)
}
\arguments{
  \item{object}{a fitted time-series model}
  \item{gof.lag}{the maximum number of lags for a Portmanteau
    goodness-of-fit test}
  \item{\dots}{further arguments to be passed to particular methods}
}
\details{
  This is a generic function. It will generally plot the residuals,
  often standadized, the autocorrelation function of the residuals, and
  the p-values of a Portmanteau test for all lags up to \code{gof.lag}.

  The method for \code{\link{arima0}} objects plots residuals scaled by
  the estimate of the innovations standard deviation, and uses the
  Ljung--Box version of the portmanteau test.
}
\value{
  None. Diagnostics are plotted.
}

\author{B. D. Ripley}

\seealso{
  \code{\link{arima0}, \code{\link{Box.test}}}
}
\note{
  \code{\link{arima0.diag}} was an earlier version using the Box--Pierce
  test which is now deprecated.
}

\examples{
data(lh)
fit <- arima0(lh, c(1,0,0))
tsdiag(fit)

## see also examples(arima0)
}
\keyword{ts}

