\name{StructTS}
\alias{StructTS}
\alias{print.StructTS}
\alias{predict.StructTS}
\title{Fit Structural Time Series}
\description{
  Fit a structural model for a time series by maximum likelihood.
}
\usage{
StructTS(x, type = c("level", "trend", "BSM"), init = NULL,
         fixed = NULL, optim.control = NULL)
}
\arguments{
  \item{x}{a univariate numeric time series. Missing values are allowed.}

  \item{type}{the class of structural model.  If omitted, a BSM is used
    for a time series with \code{frequency(x) > 1}, and a local trend
    model otherwise.}

  \item{init}{initial values of the variance parameters.}

  \item{fixed}{optional numeric vector of the same length as the total
    number of parameters.  If supplied, only \code{NA} entries in
    \code{fixed} will be varied.  Probably most useful for setting
    variances to zero.}

  \item{optim.control}{List of control parameters for
    \code{\link{optim}}.  Method \code{"L-BFGS-B"} is used.}
}
\details{
 \emph{Structural time series} models are (linear Gaussian) state-space
 models for (univariate) time series based on a decomposition of the
 series into a number of components. They are specified by a set of
 error variances, some of which may be zero.

 The simplest model is the \emph{local level} model specified by
 \code{type = "level"}.  This has an underlying level \eqn{\mu_t}{m[t]} which
 evolves by
 \deqn{\mu_{t+1} = \mu_t + \xi_t,  \qquad \xi_t \sim N(0, \sigma^2_\xi)}{%
   m[t+1] = m[t] + xi[t], xi[t] ~ N(0, sigma^2_xi)}
 The observations are
 \deqn{x_t = \mu_t + \epsilon_t, \qquad \epsilon_t \sim  N(0, \sigma^2_\epsilon)}{%
   x[t] = m[t] + eps[t], eps[t] ~  N(0, sigma^2_eps)}
 There are two parameters, \eqn{\sigma^2_\xi}{sigma^2_xi}
 and \eqn{\sigma^2_\epsilon}{sigma^2_eps}.  It is an ARIMA(0,1,1) model,
 but with restrictions on the parameter set.

 The \emph{local linear trend model}, \code{type = "trend"}, has the same
 measurement equation, but with a time-varying slope in the dynamics for
 \eqn{\mu_t}{m[t]}, given by
 \deqn{
   \mu_{t+1} = \mu_t + \nu_t + \xi_t, \qquad  \xi_t \sim N(0, \sigma^2_\xi)
 }{%
   m[t+1] = m[t] + n[t] + xi[t], xi[t] ~ N(0, sigma^2_xi)
 }
 \deqn{
   \nu_{t+1} = \nu_t + \zeta_t, \qquad \zeta_t \sim N(0, \sigma^2_\zeta)
 }{%
   n[t+1] = n[t] + zeta[t],  zeta[t] ~ N(0, sigma^2_zeta)
 }
 with three variance parameters.  It is not uncommon to find
 \eqn{\sigma^2_\zeta = 0}{sigma^2_zeta = 0} (which reduces to the local
 level model) or \eqn{\sigma^2_\xi = 0}{sigma^2_xi = 0}, which ensures a
 smooth trend.  This is a restricted ARIMA(0,2,2) model.

 The \emph{basic structural model}, \code{type = "BSM"}, is a local
 trend model with an additional seasonal component. Thus the measurement
 equation is
 \deqn{x_t = \mu_t + \gamma_t + \epsilon_t, \qquad \epsilon_t \sim  N(0, \sigma^2_\epsilon)}{%
   x[t] = m[t] + s[t] + eps[t], exp[t] ~  N(0, sigma^2_eps)}
 where \eqn{\gamma_t}{s[t]} is a seasonal component with dynamics
 \deqn{
   \gamma_{t+1} = -\gamma_t + \cdots + \gamma_{t-s+2} + \omega_t, \qquad
   \omega_t \sim N(0, \sigma^2_\omega)
 }{%
   s[t+1] = -s[t] - \dots - s[t - s + 2] + w[t],  w[t] ~ N(0, sigma^2_w)
 }
 The boundary case \eqn{\sigma^2_\omega = 0}{sigma^2_w = 0} corresponds
 to a deterministic (but arbitrary) seasonal pattern.  (This is
 sometimes known as the \sQuote{dummy variable} version of the BSM.)
}
\value{
  A list of class \code{"StructTS"} with components:

  \item{coef}{the estimated variances of the components.}
  \item{loglik}{the maximized log-likelihood.  Note that as all these
    models are non-stationary this includes a diffuse prior for some
    observations and hence is not comparable with \code{\link{arima}}
    nor different types of structural models.}
  \item{data}{the time series \code{x}.}
  \item{residuals}{the standardized residuals.}
  \item{fitted}{a multiple time series with one component for the level,
    slope and seasonal components, estimated contemporaneously (that is
    at time \eqn{t} and not at the end of the series).}
  \item{call}{the matched call.}
  \item{series}{the name of the series \code{x}.}
  \item{code}{the \code{convergence} code returned by \code{\link{optim}}.}
  \item{model, model0}{Lists representing the Kalman Filter used in the
    fitting.  See \code{\link{KalmanLike}}.  \code{model0} is the
    initial state of the filter, \code{model} its final state.}
  \item{xtsp}{the \code{tsp} attributes of \code{x}.}
}
\references{
  Brockwell, P. J. & Davis, R. A. (1996).
  \emph{Introduction to Time Series and Forecasting}.
  Springer, New York.
  Sections 8.2 and 8.5.

  Durbin, J. and Koopman, S. J. (2001) \emph{Time Series Analysis by
    State Space Methods.}  Oxford University Press.

  Harvey, A. C. (1989)
  \emph{Forecasting, Structural Time Series Models and the Kalman Filter}.
  Cambridge University Press.

  Harvey, A. C. (1993) \emph{Time Series Models}.
  2nd Edition, Harvester Wheatsheaf.
}
\note{
  Optimization of structural models is a lot harder than many of the
  references admit. For example, the \code{\link[datasets]{AirPassengers}} data
  are considered in Brockwell \& Davis (1996): their solution appears to
  be a local maximum, but nowhere near as good a fit as that produced by
  \code{StructTS}.  It is quite common to find fits with one or more
  variances zero, and this can include \eqn{\sigma^2_\epsilon}{sigma^2_eps}.
}

\seealso{
  \code{\link{KalmanLike}}, \code{\link{tsSmooth}}.
}

\examples{
## see also JohnsonJohnson, Nile and AirPassengers

trees <- window(treering, start=0)
(fit <- StructTS(trees, type = "level"))
plot(trees)
lines(fitted(fit), col = "green")
tsdiag(fit)

(fit <- StructTS(log10(UKgas), type = "BSM"))
par(mfrow = c(4, 1))
plot(log10(UKgas))
plot(cbind(fitted(fit), resids=resid(fit)), main = "UK gas consumption")

## keep some parameters fixed; trace optimizer:
StructTS(log10(UKgas), type = "BSM", fixed = c(0.1, 0.001,NA,NA),
         optim.control = list(trace=TRUE))
}
\keyword{ts}
