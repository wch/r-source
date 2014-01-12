% File src/library/stats/man/KalmanLike.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{KalmanLike}
\alias{KalmanLike}
\alias{KalmanRun}
\alias{KalmanSmooth}
\alias{KalmanForecast}
\alias{makeARIMA}
\title{Kalman Filtering}
\description{
  Use Kalman Filtering to find the (Gaussian) log-likelihood, or for
  forecasting or smoothing.
}
\usage{
KalmanLike(y, mod, nit = 0L, update = TRUE)
KalmanRun(y, mod, nit = 0L, update = TRUE)
KalmanSmooth(y, mod, nit = 0L)
KalmanForecast(n.ahead = 10L, mod, update = FALSE)

makeARIMA(phi, theta, Delta, kappa = 1e6)
}
\arguments{
  \item{y}{a univariate time series.}
  \item{mod}{A list describing the state-space model: see \sQuote{Details}.}
  \item{nit}{The time at which the initialization is computed.
    \code{nit = 0L} implies that the initialization is for a one-step
    prediction, so \code{Pn} should not be computed at the first step.}
  \item{update}{If \code{TRUE} the \code{mod} object will be modified to
    the state at the end of fitting/prediction.}
  \item{n.ahead}{The number of steps ahead for which prediction is
    required.}
  
  \item{phi, theta}{numeric vectors of length \eqn{\ge 0} giving AR
    and MA parameters.}
  \item{Delta}{vector of differencing coefficients, so an ARMA model is
    fitted to \code{y[t] - Delta[1]*y[t-1] - \dots}.}
  \item{kappa}{the prior variance (as a multiple of the innovations
    variance) for the past observations in a differenced model.}
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
    \item{\code{P}}{the current estimate of the state uncertainty matrix}
    \item{\code{Pn}}{the estimate at time \eqn{t-1} of the state
      uncertainty matrix (not updated by \code{KalmanForecast}.}
  }

  \code{KalmanSmooth} is the workhorse function for \code{\link{tsSmooth}}.

  \code{makeARIMA} constructs the state-space model for an ARIMA model.
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

  In particular, \code{KalmanLike} alters the objects passed as
  the elements \code{a}, \code{P} and \code{Pn} of \code{mod}, so these
  should not be shared.  Use \code{update = FALSE} to prevent this.
}

\references{
  Durbin, J. and Koopman, S. J. (2001) \emph{Time Series Analysis by
    State Space Methods.}  Oxford University Press.
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
for(y in 1:3) {
  pr <- KalmanForecast(4, fit3$model, TRUE)
  print(list(pred = pr$pred + fit3$coef[4], 
             se = sqrt(pr$var * fit3$sigma2)))
}
}

 \keyword{ts}
