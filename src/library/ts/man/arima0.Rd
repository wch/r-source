\name{arima0}
\alias{arima0}
\alias{print.arima0}
\alias{predict.arima0}
\title{ARIMA Modelling of Time Series -- Preliminary Version}
\usage{
arima0(x, order = c(0, 0, 0),
       seasonal = list(order = c(0, 0, 0), period = NA),
       xreg = NULL, include.mean, na.action = na.fail,
       delta = 0.01, transform.pars = 2)
predict(arima0.obj, n.ahead = 1, newxreg, se.fit = TRUE)
}
\arguments{
 \item{x}{a univariate time series}

 \item{order}{A specification of the non-seasonal part of the ARIMA
     model: the three components are (p, d, q), the AR order, the degree
     of differencing and the MA order.}

 \item{seasonal}{A specification of the seasonal part of the ARIMA
     model, plus the period (which defaults to \code{frequency(x)}).}

 \item{xreg}{Optionally, a vector or matrix of external regressors,
     which must have the same number of rows as \code{x}.}

 \item{include.mean}{Should the ARIMA model include
     a mean term? The default is \code{TRUE} for undifferenced series,
     \code{FALSE} for differenced ones (where a mean would not affect
     the fit nor predictions).}

 \item{na.action}{Function to be applied to remove missing values.}

 \item{delta}{A value to indicate at which point `fast recursions'
     should be used. See the Details section.}

 \item{transform.pars}{If grater than 0, the ARMA parameters are
     transformed to ensure that they remain in the region of
     invertibility. If equal to 2, the optimization is rerun on
     the original scale to find the Hessian.}

 \item{arima0.obj}{The result of an \code{arima0} fit.}

 \item{newxreg}{New values of \code{xreg} to be used for
     prediction. Must have at least \code{n.ahead} rows.}

 \item{n.ahead}{The number of steps ahead for which prediction is required.}

 \item{se.fit}{Logical: should standard errors of prediction be returned?}
}
\description{
    Fit an ARIMA model to a univariate time series by exact maximum
    likelihood, and forecast from the fitted model.
}
\details{
    The exact likelihood is computed via a state-space representation of
    the ARMA process, and the innovations and their variance found by a
    Kalman filter using the Fortran code of Gardener \emph{et al.}
    (1980).  This has the option to switch to `fast recursions' (assume
    an effectively infinite past) if the innovations variance is close
    enough to its asymptotic bound. The argument \code{delta} sets the
    tolerance: at its default value the approximation is normally
    negligible and the speed-up considerable. Exact computations can be
    ensured by setting \code{delta} to a negative value.

    The variance matrix of the estimates is found from the Hessian of
    the log-likelihood, and so may only be a rough guide, especially for
    fits close to the boundary of invertibility.

    Optimization is (currently) done by \code{\link{nlm}}. It will work
    best if the columns in \code{xreg} are roughly scaled to zero mean
    and unit variance.

    Finite-history prediction is used. This is only statistically
    efficient if the MA part of the fit is invertible, so
    \code{predict.arima0} will give a warning for non-invertible MA
    models.
}
\value{
  For \code{arima0}, a list of class \code{"arima0"} with components:

  \item{coef}{a vector of AR, MA and regression coefficients,}

  \item{sigma2}{the MLE of the innovations variance.}

  \item{var.coef}{the estimated variance matrix of the coefficients
      \code{coef}. If \code{transform.pars = 1}, only the portion
      corresponding to the untransformed parameters is returned.}

  \item{loglik}{the maximized log-likelihood (of the differenced data).}

  \item{arma}{A compact form of the specification, as a vector giving
      the number of AR, MA, seasonal AR and seasonal MA coefficients,
      plus the period and the number of non-seasonal and seasonal
      differences.}

  \item{aic}{the AIC value corresponding to the log-likelihood.}

  \item{resid}{the residuals.}

  \item{call}{the matched call.}

  \item{series}{the name of the series \code{x}.}

  \item{convergence}{the \code{code} returned by \code{\link{nlm}}.}

  For \code{predict.arima0}, a time series of predictions, or if
  \code{se.fit = TRUE}, a list with components \code{pred}, the
  predictions, and \code{se}, the estimated standard errors. Both
  components are time series.
}
\references{
    Brockwell, P. J. and Davis, R. A. (1996) \emph{Introduction to Time
	Series and Forecasting.} Springer, New York. Sections 3.3 and 8.3.

    Gardener, G, Harvey, A. C. and Phillips, G. D. A. (1980) Algorithm
    AS154. An algorithm for exact maximum likelihood estimation of
    autoregressive-moving average models by means of Kalman filtering.
    \emph{Applied Statistics} \bold{29}, 311-322.

    Harvey, A. C. (1993) \emph{Time Series Models},
    2nd Edition, Harvester Wheatsheaf, section 4.4.

    Harvey, A. C. and McKenzie, C. R. (1982) Algorithm AS182.
    An algorithm for finite sample prediction from ARIMA processes.
    \emph{Applied Statistics} \bold{31}, 180-187.
}

\author{B.D. Ripley}
\note{
    This is a preliminary version, and will be replaced in due course.

    The standard errors of prediction exclude the uncertainty in the
    estimation of the ARMA model and the regression coefficients.

    The results are likely to be different from S-PLUS's
    \code{arima.mle}, which computes a conditional likelihood and does
    not include a mean in the model. Further, the convention used by
    \code{arima.mle} reverses the signs of the MA coefficients.
}

\seealso{\code{\link{ar}}}

\examples{
data(lh)
arima0(lh, order=c(1,0,0))
arima0(lh, order=c(3,0,0))
arima0(lh, order=c(1,0,1))
predict(arima0(lh, order=c(3,0,0)), n.ahead=12)

data(USAccDeaths)
fit <- arima0(USAccDeaths, order=c(0,1,1), seasonal=list(order=c(0,1,1)))
fit
predict(fit, n.ahead=6)

data(LakeHuron)
arima0(LakeHuron, order=c(2,0,0), xreg=1:98)
}
\keyword{ts}
