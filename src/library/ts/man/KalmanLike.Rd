\name{KalmanLike}
\alias{KalmanLike}
\alias{KalmanRun}
\alias{KalmanForecast}
\alias{makeARIMA}
\title{Kalman Filtering}
\description{
  Use Kalman Filtering to find the (Gaussian) log-likelihood, or for
  forecasting.
}
\usage{
KalmanLike(y, mod, nit = 0)
KalmanRun(y, mod, nit = 0)
KalmanForecast(n.ahead = 10, mod)
makeARIMA(phi, theta, Delta, kappa = 1e6)
}
\arguments{
  \item{y}{a univariate time series.}
  \item{mod}{A list describing the state-space model: see Details.}
  \item{nit}{The time at which the initialization is computed.
    \code{nit = 0} implies that the initialization is for a one-step
    prediction, so \code{Pn} should not be computed at the first step.}
  \item{n.ahead}{The number of steps ahead for which prediction is
    required.}
  \item{phi, theta}{numeric vectors of length >=0 giving AR and MA
    parameters.}
  \item{Delta}{vector of differencing coefficients, so an ARMA model is
    fitted to \code{y[t] - Delta[1]*y[t-1] - \dots}.}
  \item{kappa}{the prior variance (as a multiple of the innovations
    variance) for the past observations in a differenced model.}
}
\details{
  These functions work with a general univariate state-space model
  with state vector \code{a}, transitions \code{a <- Ta + e}, \eqn{e ~
    N(0, \kappa Q)} and observation equation \code{y = Z'a + Reta},
  \eqn{eta ~ N(0, \kappa h)}.  The likelihood is a profile likelihood
  after estimation of \eqn{\kappa}.

  The model is specified as a list with at least components
  \describe{
    \item{\code{T}}{the transition matrix}
    \item{\code{Z}}{the observation coeficients}
    \item{\code{h}}{the observation variance}
    \item{\code{V}}{\code{RQR'}}
    \item{\code{a}}{the current state estimate}
    \item{\code{P}}{the current estimate of the state uncertainty matrix}
    \item{\code{Pn}}{the estimate at time \eqn{t-1} of the state
      uncertainty matrix}
  }
  \code{makeARIMA} constructs the state-space model for an ARIMA model.
}

\value{
  For \code{KalmanLike}, a list with components \code{Lik} (the
  log-likelihood less some constants) and \code{s2}, the estimate of
  of \eqn{\kappa}.

  For \code{KalmanRun}, a list with components \code{values}, a vector
  of length 2 giving the output of \code{KalmanLike}, \code{resid} (the
  residuals) and \code{states}, the contemporaneous state estimates,
  a matrix with one column for each time.

  For \code{KalmanForecast}, a list with components \code{pred}, the
  predictions, and \code{var}, the unscaled variances of the prediction
  errors (to be muliplied by \code{s2}).

  For \code{makeARIMA}, a model list including components for
  its arguments.
}

\references{
  Durbin, J. and Koopman, S. J. (2001) \emph{Time Series Analysis by
    State Space Methods.}  Oxford University Press.
}
\author{B. D. Ripley}

\seealso{
   \code{\link{arima}}
 }
\keyword{ts}
