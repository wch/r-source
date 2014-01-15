% File src/library/stats/man/KalmanLike.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{KalmanLike}
\alias{KalmanLike}%-> ../R/Kalman.R
\alias{KalmanRun}
\alias{KalmanSmooth}
\alias{KalmanForecast}
\alias{makeARIMA}%-> ../R/arima.R
\title{Kalman Filtering}
\description{
  Use Kalman Filtering to find the (Gaussian) log-likelihood, or for
  forecasting or smoothing.
}
\usage{
KalmanLike(y, mod, nit = 0L, update = FALSE)
KalmanRun(y, mod, nit = 0L, update = FALSE)
KalmanSmooth(y, mod, nit = 0L)
KalmanForecast(n.ahead = 10L, mod, update = FALSE)

makeARIMA(phi, theta, Delta, kappa = 1e6,
          SSinit = c("Gardner1980", "Rossignol2011"),
          tol = .Machine$double.eps)
}
\arguments{
  \item{y}{a univariate time series.}
  \item{mod}{a list describing the state-space model: see \sQuote{Details}.}
  \item{nit}{the time at which the initialization is computed.
    \code{nit = 0L} implies that the initialization is for a one-step
    prediction, so \code{Pn} should not be computed at the first step.}
  \item{update}{if \code{TRUE} the update \code{mod} object will be
    returned as attribute \code{"mod"} of the result.}
  \item{n.ahead}{the number of steps ahead for which prediction is
    required.}
  
  \item{phi, theta}{numeric vectors of length \eqn{\ge 0} giving AR
    and MA parameters.}
  \item{Delta}{vector of differencing coefficients, so an ARMA model is
    fitted to \code{y[t] - Delta[1]*y[t-1] - \dots}.}
  \item{kappa}{the prior variance (as a multiple of the innovations
    variance) for the past observations in a differenced model.}
  \item{SSinit}{a string specifying the algorithm to compute the
    \code{Pn} part of the state-space initialization; see
    \sQuote{Details}.}
  \item{tol}{tolerance eventually passed to \code{\link{solve.default}}
    when \code{SSinit = "Rossignol2011"}.}
}
\details{
  These functions work with a general univariate state-space model
  with state vector \samp{a}, transitions \samp{a <- T a + R e},
  \eqn{e \sim {\cal N}(0, \kappa Q)}{e ~ N(0, kappa Q)} and observation
  equation \samp{y = Z'a + eta},
  \eqn{(eta\equiv\eta), \eta \sim {\cal N}(0, \kappa h)}{eta ~ N(0, kappa h)}.
  The likelihood is a profile likelihood after estimation of
  \eqn{\kappa}{kappa}.

  The model is specified as a list with at least components
  \describe{
    \item{\code{T}}{the transition matrix}
    \item{\code{Z}}{the observation coefficients}
    \item{\code{h}}{the observation variance}
    \item{\code{V}}{\samp{RQR'}}
    \item{\code{a}}{the current state estimate}
    \item{\code{P}}{the current estimate of the state uncertainty matrix \eqn{Q}}
    \item{\code{Pn}}{the estimate at time \eqn{t-1} of the state
      uncertainty matrix \eqn{Q} (not updated by \code{KalmanForecast}.}
  }

  \code{KalmanSmooth} is the workhorse function for \code{\link{tsSmooth}}.

  \code{makeARIMA} constructs the state-space model for an ARIMA model,
  see also \code{\link{arima}}.

  The state-space initialization has used Gardner et al's method
  (\code{SSinit = "Gardner1980"}), as only method for years.  However,
  that suffers sometimes from deficiencies when close to non-stationarity.
  For this reason, it may be replaced as default in the future and only
  kept for reproducibility reasons.  Explicit specification of
  \code{SSinit} is therefore recommended, notably also in
  \code{\link{arima}()}.
  The \code{"Rossignol2011"} method has been proposed and partly
  documented by Raphael Rossignol, Univ. Grenoble, on 2011-09-20 (see
  PR#14682, below), and later been ported to C by Matwey V. Kornilov.
  It computes the covariance matrix of
  \eqn{(X_{t-1},...,X_{t-p},Z_t,...,Z_{t-q})}
  by the method of difference equations (page 93 of Brockwell and Davis),
  apparently suggested by a referee of  Gardner et al. (see p.314 of
  their paper).
}

\value{
  For \code{KalmanLike}, a list with components \code{Lik} (the
  log-likelihood less some constants) and \code{s2}, the estimate of
  \eqn{\kappa}{kappa}.

  For \code{KalmanRun}, a list with components \code{values}, a vector
  of length 2 giving the output of \code{KalmanLike}, \code{resid} (the
  residuals) and \code{states}, the contemporaneous state estimates,
  a matrix with one row for each observation time.

  For \code{KalmanSmooth}, a list with two components.
  Component \code{smooth} is a \code{n} by \code{p} matrix of state
  estimates based on all the observations, with one row for each time.
  Component \code{var} is a \code{n} by \code{p} by \code{p} array of
  variance matrices.

  For \code{KalmanForecast}, a list with components \code{pred}, the
  predictions, and \code{var}, the unscaled variances of the prediction
  errors (to be multiplied by \code{s2}).

  For \code{makeARIMA}, a model list including components for
  its arguments.
}

\section{Warning}{
  These functions are designed to be called from other functions which
  check the validity of the arguments passed, so very little checking is
  done.
}

\references{
  Durbin, J. and Koopman, S. J. (2001) \emph{Time Series Analysis by
    State Space Methods.}  Oxford University Press.

  Gardner, G, Harvey, A. C. and Phillips, G. D. A. (1980) Algorithm
  AS154. An algorithm for exact maximum likelihood estimation of
  autoregressive-moving average models by means of Kalman filtering.
  \emph{Applied Statistics} \bold{29}, 311--322.

  R bug report PR#14682 (2011-2013)
  \url{https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=14682}.
}

\seealso{
  \code{\link{arima}}, \code{\link{StructTS}}. \code{\link{tsSmooth}}.
}

\examples{
## an ARIMA fit
fit3 <- arima(presidents, c(3, 0, 0))
predict(fit3, 12)
## reconstruct this
pr <- KalmanForecast(12, fit3$model)
pr$pred + fit3$coef[4]
sqrt(pr$var * fit3$sigma2)
## and now do it year by year
mod <- fit3$model
for(y in 1:3) {
  pr <- KalmanForecast(4, mod, TRUE)
  print(list(pred = pr$pred + fit3$coef["intercept"], 
             se = sqrt(pr$var * fit3$sigma2)))
  mod <- attr(pr, "mod")
}
}

\keyword{ts}
