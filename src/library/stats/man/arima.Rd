% File src/library/stats/man/arima.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{arima}
\alias{arima}
\concept{ARMA}
\title{ARIMA Modelling of Time Series}
\description{
  Fit an ARIMA model to a univariate time series.
}
\usage{
arima(x, order = c(0L, 0L, 0L),
      seasonal = list(order = c(0L, 0L, 0L), period = NA),
      xreg = NULL, include.mean = TRUE,
      transform.pars = TRUE,
      fixed = NULL, init = NULL,
      method = c("CSS-ML", "ML", "CSS"), n.cond,
      SSinit = c("Gardner1980", "Rossignol2011"),
      optim.method = "BFGS",
      optim.control = list(), kappa = 1e6)
}
\arguments{
  \item{x}{a univariate time series}

  \item{order}{A specification of the non-seasonal part of the ARIMA
    model: the three integer components \eqn{(p, d, q)} are the AR order, the
    degree of differencing, and the MA order.}

  \item{seasonal}{A specification of the seasonal part of the ARIMA
    model, plus the period (which defaults to \code{frequency(x)}).
    This should be a list with components \code{order} and
    \code{period}, but a specification of just a numeric vector of
    length 3 will be turned into a suitable list with the specification
    as the \code{order}.}

  \item{xreg}{Optionally, a vector or matrix of external regressors,
    which must have the same number of rows as \code{x}.}

  \item{include.mean}{Should the ARMA model include a mean/intercept term?  The
    default is \code{TRUE} for undifferenced series, and it is ignored
    for ARIMA models with differencing.}

  \item{transform.pars}{logical; if true, the AR parameters are
    transformed to ensure that they remain in the region of
    stationarity.  Not used for \code{method = "CSS"}.  For
    \code{method = "ML"}, it has been advantageous to set
    \code{transform.pars = FALSE} in some cases, see also \code{fixed}.}

  \item{fixed}{optional numeric vector of the same length as the total
    number of parameters.  If supplied, only \code{NA} entries in
    \code{fixed} will be varied.  \code{transform.pars = TRUE}
    will be overridden (with a warning) if any AR parameters are fixed.
    It may be wise to set \code{transform.pars = FALSE} when fixing
    MA parameters, especially near non-invertibility.
  }

  \item{init}{optional numeric vector of initial parameter
    values.  Missing values will be filled in, by zeroes except for
    regression coefficients.  Values already specified in \code{fixed}
    will be ignored.}

  \item{method}{fitting method: maximum likelihood or minimize
    conditional sum-of-squares.  The default (unless there are missing
    values) is to use conditional-sum-of-squares to find starting
    values, then maximum likelihood.  Can be abbreviated.}

  \item{n.cond}{only used if fitting by conditional-sum-of-squares: the
    number of initial observations to ignore.  It will be ignored if
    less than the maximum lag of an AR term.}

  \item{SSinit}{a string specifying the algorithm to compute the
    state-space initialization of the likelihood; see
    \code{\link{KalmanLike}} for details.   Can be abbreviated.}

  \item{optim.method}{The value passed as the \code{method} argument to
    \code{\link{optim}}.}

  \item{optim.control}{List of control parameters for \code{\link{optim}}.}

  \item{kappa}{the prior variance (as a multiple of the innovations
    variance) for the past observations in a differenced model.  Do not
    reduce this.}
}
\details{
  Different definitions of ARMA models have different signs for the
  AR and/or MA coefficients.  The definition used here has

  \deqn{X_t= a_1X_{t-1}+ \cdots+ a_pX_{t-p} + e_t + b_1e_{t-1}+\cdots+ b_qe_{t-q}
  }{X[t] = a[1]X[t-1] + \dots + a[p]X[t-p] + e[t] + b[1]e[t-1] + \dots + b[q]e[t-q]}

  and so the MA coefficients differ in sign from those of S-PLUS.
  Further, if \code{include.mean} is true (the default for an ARMA
  model), this formula applies to \eqn{X - m} rather than \eqn{X}.  For
  ARIMA models with differencing, the differenced series follows a
  zero-mean ARMA model. If am \code{xreg} term is included, a linear
  regression (with a constant term if \code{include.mean} is true and
  there is no differencing) is fitted with an ARMA model for the error
  term.

  The variance matrix of the estimates is found from the Hessian of
  the log-likelihood, and so may only be a rough guide.

  Optimization is done by \code{\link{optim}}.  It will work
  best if the columns in \code{xreg} are roughly scaled to zero mean
  and unit variance, but does attempt to estimate suitable scalings.
}
\section{Fitting methods}{
  The exact likelihood is computed via a state-space representation of
  the ARIMA process, and the innovations and their variance found by a
  Kalman filter.  The initialization of the differenced ARMA process uses
  stationarity and is based on Gardner \emph{et al} (1980).  For a
  differenced process the non-stationary components are given a diffuse
  prior (controlled by \code{kappa}).  Observations which are still
  controlled by the diffuse prior (determined by having a Kalman gain of
  at least \code{1e4}) are excluded from the likelihood calculations.
  (This gives comparable results to \code{\link{arima0}} in the absence
  of missing values, when the observations excluded are precisely those
  dropped by the differencing.)

  Missing values are allowed, and are handled exactly in method \code{"ML"}.

  If \code{transform.pars} is true, the optimization is done using an
  alternative parametrization which is a variation on that suggested by
  Jones (1980) and ensures that the model is stationary.  For an AR(p)
  model the parametrization is via the inverse tanh of the partial
  autocorrelations: the same procedure is applied (separately) to the
  AR and seasonal AR terms.  The MA terms are not constrained to be
  invertible during optimization, but they will be converted to
  invertible form after optimization if \code{transform.pars} is true.

  Conditional sum-of-squares is provided mainly for expositional
  purposes.  This computes the sum of squares of the fitted innovations
  from observation \code{n.cond} on, (where \code{n.cond} is at least
  the maximum lag of an AR term), treating all earlier innovations to
  be zero.  Argument \code{n.cond} can be used to allow comparability
  between different fits.  The \sQuote{part log-likelihood} is the first
  term, half the log of the estimated mean square.  Missing values
  are allowed, but will cause many of the innovations to be missing.

  When regressors are specified, they are orthogonalized prior to
  fitting unless any of the coefficients is fixed.  It can be helpful to
  roughly scale the regressors to zero mean and unit variance.
}
\value{
  A list of class \code{"Arima"} with components:

  \item{coef}{a vector of AR, MA and regression coefficients, which can
    be extracted by the \code{\link{coef}} method.}

  \item{sigma2}{the MLE of the innovations variance.}

  \item{var.coef}{the estimated variance matrix of the coefficients
    \code{coef}, which can be extracted by the \code{\link{vcov}} method.}

  \item{loglik}{the maximized log-likelihood (of the differenced data),
    or the approximation to it used.}

  \item{arma}{A compact form of the specification, as a vector giving
    the number of AR, MA, seasonal AR and seasonal MA coefficients,
    plus the period and the number of non-seasonal and seasonal
    differences.}

  \item{aic}{the AIC value corresponding to the log-likelihood. Only
    valid for \code{method = "ML"} fits.}

  \item{residuals}{the fitted innovations.}

  \item{call}{the matched call.}

  \item{series}{the name of the series \code{x}.}

  \item{code}{the convergence value returned by \code{\link{optim}}.}

  \item{n.cond}{the number of initial observations not used in the fitting.}

  \item{nobs}{the number of \dQuote{used} observations for the fitting,
    can also be extracted via \code{\link{nobs}()} and is used by
    \code{\link{BIC}}.}

  \item{model}{A list representing the Kalman Filter used in the
    fitting.  See \code{\link{KalmanLike}}.}
}
\references{
  Brockwell, P. J. and Davis, R. A. (1996) \emph{Introduction to Time
    Series and Forecasting.} Springer, New York. Sections 3.3 and 8.3.

  Durbin, J. and Koopman, S. J. (2001) \emph{Time Series Analysis by
    State Space Methods.}  Oxford University Press.

  Gardner, G, Harvey, A. C. and Phillips, G. D. A. (1980) Algorithm
  AS154. An algorithm for exact maximum likelihood estimation of
  autoregressive-moving average models by means of Kalman filtering.
  \emph{Applied Statistics} \bold{29}, 311--322.

  Harvey, A. C. (1993) \emph{Time Series Models},
  2nd Edition, Harvester Wheatsheaf, sections 3.3 and 4.4.

  Jones, R. H. (1980) Maximum likelihood fitting of ARMA models to time
  series with missing observations. \emph{Technometrics} \bold{22} 389--395.

  Ripley, B. D. (2002) Time series in \R 1.5.0.
  \emph{R News}, \bold{2/2}, 2--7.
  \url{https://www.r-project.org/doc/Rnews/Rnews_2002-2.pdf}
}

\note{
  The results are likely to be different from S-PLUS's
  \code{arima.mle}, which computes a conditional likelihood and does
  not include a mean in the model.  Further, the convention used by
  \code{arima.mle} reverses the signs of the MA coefficients.

  \code{arima} is very similar to \code{\link{arima0}} for
  ARMA models or for differenced models without missing values,
  but handles differenced models with missing values exactly.
  It is somewhat slower than \code{arima0}, particularly for seasonally
  differenced models.
}

\seealso{
  \code{\link{predict.Arima}}, \code{\link{arima.sim}} for simulating
  from an ARIMA model, \code{\link{tsdiag}}, \code{\link{arima0}},
  \code{\link{ar}}
}

\examples{
arima(lh, order = c(1,0,0))
arima(lh, order = c(3,0,0))
arima(lh, order = c(1,0,1))

arima(lh, order = c(3,0,0), method = "CSS")

arima(USAccDeaths, order = c(0,1,1), seasonal = list(order = c(0,1,1)))
arima(USAccDeaths, order = c(0,1,1), seasonal = list(order = c(0,1,1)),
      method = "CSS") # drops first 13 observations.
# for a model with as few years as this, we want full ML

arima(LakeHuron, order = c(2,0,0), xreg = time(LakeHuron) - 1920)

## presidents contains NAs
## graphs in example(acf) suggest order 1 or 3
require(graphics)
(fit1 <- arima(presidents, c(1, 0, 0)))
nobs(fit1)
tsdiag(fit1)
(fit3 <- arima(presidents, c(3, 0, 0)))  # smaller AIC
tsdiag(fit3)
BIC(fit1, fit3)
## compare a whole set of models; BIC() would choose the smallest
AIC(fit1, arima(presidents, c(2,0,0)),
          arima(presidents, c(2,0,1)), # <- chosen (barely) by AIC
    fit3, arima(presidents, c(3,0,1)))

## An example of ARIMA forecasting:
predict(fit3, 3)
}
\keyword{ts}
