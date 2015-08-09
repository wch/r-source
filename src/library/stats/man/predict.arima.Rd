% File src/library/stats/man/predict.arima.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{predict.Arima}
\alias{predict.Arima}
\title{Forecast from ARIMA fits}
\description{
  Forecast from models fitted by \code{\link{arima}}.
}
\usage{
\method{predict}{Arima}(object, n.ahead = 1, newxreg = NULL,
        se.fit = TRUE, \dots)
}
\arguments{
  \item{object}{The result of an \code{arima} fit.}
  \item{n.ahead}{The number of steps ahead for which prediction is required.}
  \item{newxreg}{New values of \code{xreg} to be used for
    prediction. Must have at least \code{n.ahead} rows.}
  \item{se.fit}{Logical: should standard errors of prediction be returned?}
  \item{\dots}{arguments passed to or from other methods.}
}
\details{
  Finite-history prediction is used, via \code{\link{KalmanForecast}}.
  This is only statistically efficient if the MA part of the fit is
  invertible, so \code{predict.Arima} will give a warning for
  non-invertible MA models.

  The standard errors of prediction exclude the uncertainty in the
  estimation of the ARMA model and the regression coefficients.
  According to Harvey (1993, pp.\sspace{}58--9) the effect is small.
}
\value{
  A time series of predictions, or if \code{se.fit = TRUE}, a list
  with components \code{pred}, the predictions, and \code{se},
  the estimated standard errors.  Both components are time series.
}
\references{
  Durbin, J. and Koopman, S. J. (2001) \emph{Time Series Analysis by
    State Space Methods.}  Oxford University Press.

  Harvey, A. C. and McKenzie, C. R. (1982) Algorithm AS182.
  An algorithm for finite sample prediction from ARIMA processes.
  \emph{Applied Statistics} \bold{31}, 180--187.

  Harvey, A. C. (1993) \emph{Time Series Models},
  2nd Edition, Harvester Wheatsheaf, sections 3.3 and 4.4.
}

\seealso{
  \code{\link{arima}}
}

\examples{
od <- options(digits = 5) # avoid too much spurious accuracy
predict(arima(lh, order = c(3,0,0)), n.ahead = 12)

(fit <- arima(USAccDeaths, order = c(0,1,1),
              seasonal = list(order = c(0,1,1))))
predict(fit, n.ahead = 6)
options(od)
}
\keyword{ts}
