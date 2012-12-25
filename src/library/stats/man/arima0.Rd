% File src/library/stats/man/arima0.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{arima0}
\alias{arima0}
\alias{print.arima0}
\alias{predict.arima0}
\concept{ARNA}
\title{ARIMA Modelling of Time Series -- Preliminary Version}
\description{
  Fit an ARIMA model to a univariate time series, and forecast from
  the fitted model.
}
\usage{
arima0(x, order = c(0, 0, 0),
       seasonal = list(order = c(0, 0, 0), period = NA),
       xreg = NULL, include.mean = TRUE, delta = 0.01,
       transform.pars = TRUE, fixed = NULL, init = NULL,
       method = c("ML", "CSS"), n.cond, optim.control = list())

\method{predict}{arima0}(object, n.ahead = 1, newxreg, se.fit = TRUE, \dots)
}
\arguments{
  \item{x}{a univariate time series}

  \item{order}{A specification of the non-seasonal part of the ARIMA
    model: the three components \eqn{(p, d, q)} are the AR order, the
    degree of differencing, and the MA order.}

  \item{seasonal}{A specification of the seasonal part of the ARIMA
    model, plus the period (which defaults to \code{frequency(x)}).
    This should be a list with components \code{order} and
    \code{period}, but a specification of just a numeric vector of
    length 3 will be turned into a suitable list with the specification
    as the \code{order}.}

  \item{xreg}{Optionally, a vector or matrix of external regressors,
    which must have the same number of rows as \code{x}.}

  \item{include.mean}{Should the ARIMA model include
    a mean term? The default is \code{TRUE} for undifferenced series,
    \code{FALSE} for differenced ones (where a mean would not affect
    the fit nor predictions).}

  \item{delta}{A value to indicate at which point \sQuote{fast
      recursions} should be used.  See the \sQuote{Details} section.}

  \item{transform.pars}{Logical.  If true, the AR parameters are
    transformed to ensure that they remain in the region of
    stationarity.  Not used for \code{method = "CSS"}.}

  \item{fixed}{optional numeric vector of the same length as the total
    number of parameters.  If supplied, only \code{NA} entries in
    \code{fixed} will be varied.  \code{transform.pars = TRUE}
    will be overridden (with a warning) if any ARMA parameters are
    fixed.}

  \item{init}{optional numeric vector of initial parameter
    values.  Missing values will be filled in, by zeroes except for
    regression coefficients.  Values already specified in \code{fixed}
    will be ignored.}

  \item{method}{Fitting method: maximum likelihood or minimize
    conditional sum-of-squares.}

  \item{n.cond}{Only used if fitting by conditional-sum-of-squares: the
    number of initial observations to ignore.  It will be ignored if
    less than the maximum lag of an AR term.}

  \item{optim.control}{List of control parameters for \code{\link{optim}}.}

  \item{object}{The result of an \code{arima0} fit.}

  \item{newxreg}{New values of \code{xreg} to be used for
    prediction. Must have at least \code{n.ahead} rows.}

  \item{n.ahead}{The number of steps ahead for which prediction is required.}

  \item{se.fit}{Logical: should standard errors of prediction be returned?}

  \item{\dots}{arguments passed to or from other methods.}
}
\details{
  Different definitions of ARMA models have different signs for the
  AR and/or MA coefficients. The definition here has

  \deqn{X_t = a_1X_{t-1} + \cdots + a_pX_{t-p} + e_t + b_1e_{t-1} + \dots + b_qe_{t-q}}{X[t] = a[1]X[t-1] + \dots + a[p]X[t-p] + e[t] + b[1]e[t-1] + \dots + b[q]e[t-q]}

  and so the MA coefficients differ in sign from those of
  S-PLUS.  Further, if \code{include.mean} is true, this formula
  applies to \eqn{X-m} rather than \eqn{X}.  For ARIMA models with
  differencing, the differenced series follows a zero-mean ARMA model.

  The variance matrix of the estimates is found from the Hessian of
  the log-likelihood, and so may only be a rough guide, especially for
  fits close to the boundary of invertibility.

  Optimization is done by \code{\link{optim}}. It will work
  best if the columns in \code{xreg} are roughly scaled to zero mean
  and unit variance, but does attempt to estimate suitable scalings.

  Finite-history prediction is used. This is only statistically
  efficient if the MA part of the fit is invertible, so
  \code{predict.arima0} will give a warning for non-invertible MA
  models.
}
\section{Fitting methods}{
  The exact likelihood is computed via a state-space representation of
  the ARMA process, and the innovations and their variance found by a
  Kalman filter based on Gardner \emph{et al.} (1980).  This has
  the option to switch to \sQuote{fast recursions} (assume an
  effectively infinite past) if the innovations variance is close
  enough to its asymptotic bound. The argument \code{delta} sets the
  tolerance: at its default value the approximation is normally
  negligible and the speed-up considerable.  Exact computations can be
  ensured by setting \code{delta} to a negative value.

  If \code{transform.pars} is true, the optimization is done using an
  alternative parametrization which is a variation on that suggested by
  Jones (1980) and ensures that the model is stationary.  For an AR(p)
  model the parametrization is via the inverse tanh of the partial
  autocorrelations: the same procedure is applied (separately) to the
  AR and seasonal AR terms.  The MA terms are also constrained to be
  invertible during optimization by the same transformation if
  \code{transform.pars} is true.  Note that the MLE for MA terms does
  sometimes occur for MA polynomials with unit roots: such models can be
  fitted by using \code{transform.pars = FALSE} and specifying a good
  set of initial values (often obtainable from a fit with
  \code{transform.pars = TRUE}).

  Missing values are allowed, but any missing values
  will force \code{delta} to be ignored and full recursions used.
  Note that missing values will be propagated by differencing, so the
  procedure used in this function is not fully efficient in that case.

  Conditional sum-of-squares is provided mainly for expositional
  purposes.  This computes the sum of squares of the fitted innovations
  from observation
  \code{n.cond} on, (where \code{n.cond} is at least the maximum lag of
  an AR term), treating all earlier innovations to be zero.  Argument
  \code{n.cond} can be used to allow comparability between different
  fits.  The \sQuote{part log-likelihood} is the first term, half the
  log of the estimated mean square.  Missing values are allowed, but
  will cause many of the innovations to be missing.

  When regressors are specified, they are orthogonalized prior to
  fitting unless any of the coefficients is fixed.  It can be helpful to
  roughly scale the regressors to zero mean and unit variance.
}
\value{
  For \code{arima0}, a list of class \code{"arima0"} with components:

  \item{coef}{a vector of AR, MA and regression coefficients,}

  \item{sigma2}{the MLE of the innovations variance.}

  \item{var.coef}{the estimated variance matrix of the coefficients
    \code{coef}.}

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

  \item{convergence}{the value returned by \code{\link{optim}}.}

  \item{n.cond}{the number of initial observations not used in the fitting.}

  For \code{predict.arima0}, a time series of predictions, or if
  \code{se.fit = TRUE}, a list with components \code{pred}, the
  predictions, and \code{se}, the estimated standard errors. Both
  components are time series.
}
\references{
  Brockwell, P. J. and Davis, R. A. (1996) \emph{Introduction to Time
    Series and Forecasting.} Springer, New York. Sections 3.3 and 8.3.

  Gardner, G, Harvey, A. C. and Phillips, G. D. A. (1980) Algorithm
  AS154. An algorithm for exact maximum likelihood estimation of
  autoregressive-moving average models by means of Kalman filtering.
  \emph{Applied Statistics} \bold{29}, 311--322.

  Harvey, A. C. (1993) \emph{Time Series Models},
  2nd Edition, Harvester Wheatsheaf, sections 3.3 and  4.4.

  Harvey, A. C. and McKenzie, C. R. (1982) Algorithm AS182.
  An algorithm for finite sample prediction from ARIMA processes.
  \emph{Applied Statistics} \bold{31}, 180--187.

  Jones, R. H. (1980) Maximum likelihood fitting of ARMA models to time
  series with missing observations. \emph{Technometrics} \bold{22} 389--395.
}

\note{
  This is a preliminary version, and will be replaced by \code{\link{arima}}.

  The standard errors of prediction exclude the uncertainty in the
  estimation of the ARMA model and the regression coefficients.

  The results are likely to be different from S-PLUS's
  \code{arima.mle}, which computes a conditional likelihood and does
  not include a mean in the model.  Further, the convention used by
  \code{arima.mle} reverses the signs of the MA coefficients.
}

\seealso{\code{\link{arima}}, \code{\link{ar}}, \code{\link{tsdiag}}}

\examples{
\dontrun{arima0(lh, order = c(1,0,0))}
arima0(lh, order = c(3,0,0))
arima0(lh, order = c(1,0,1))
predict(arima0(lh, order = c(3,0,0)), n.ahead = 12)

arima0(lh, order = c(3,0,0), method = "CSS")

# for a model with as few years as this, we want full ML
(fit <- arima0(USAccDeaths, order = c(0,1,1),
               seasonal = list(order=c(0,1,1)), delta = -1))
predict(fit, n.ahead = 6)

arima0(LakeHuron, order = c(2,0,0), xreg = time(LakeHuron)-1920)
\dontrun{
## presidents contains NAs
## graphs in example(acf) suggest order 1 or 3
(fit1 <- arima0(presidents, c(1, 0, 0), delta = -1))  # avoid warning
tsdiag(fit1)
(fit3 <- arima0(presidents, c(3, 0, 0), delta = -1))  # smaller AIC
tsdiag(fit3)}
}
\keyword{ts}
