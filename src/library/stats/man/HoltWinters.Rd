% File src/library/stats/man/HoltWinters.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{HoltWinters}
\alias{HoltWinters}
\alias{print.HoltWinters}
\alias{residuals.HoltWinters}
\title{Holt-Winters Filtering}
\description{
  Computes Holt-Winters Filtering of a given time series.
  Unknown parameters are determined by minimizing the squared
  prediction error.
}
\usage{
HoltWinters(x, alpha = NULL, beta = NULL, gamma = NULL,
            seasonal = c("additive", "multiplicative"),
            start.periods = 2, l.start = NULL, b.start = NULL,
            s.start = NULL,
            optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
            optim.control = list())
}
\arguments{
  \item{x}{An object of class \code{ts}}
  \item{alpha}{\eqn{alpha} parameter of Holt-Winters Filter.}
  \item{beta}{\eqn{beta} parameter of Holt-Winters Filter. If set to
    \code{FALSE}, the function will do exponential smoothing.}
  \item{gamma}{\eqn{gamma} parameter used for the seasonal component.
    If set to \code{FALSE}, an non-seasonal model is fitted.}
  \item{seasonal}{Character string to select an \code{"additive"}
    (the default) or \code{"multiplicative"} seasonal model. The first
    few characters are sufficient. (Only takes effect if
    \code{gamma} is non-zero).}
  \item{start.periods}{Start periods used in the autodetection of start
    values. Must be at least 2.}
  \item{l.start}{Start value for level (a[0]).}
  \item{b.start}{Start value for trend (b[0]).}
  \item{s.start}{Vector of start values for the seasonal component
    (\eqn{s_1[0] \ldots s_p[0]}{s_1[0] \dots s_p[0]})}
  \item{optim.start}{Vector with named components \code{alpha},
    \code{beta}, and \code{gamma} containing the starting values for the
    optimizer. Only the values needed must be specified.  Ignored in the
    one-parameter case.}
  \item{optim.control}{Optional list with additional control parameters
    passed to \code{optim} if this is used.  Ignored in the
    one-parameter case.}
}
\details{
  The additive Holt-Winters prediction function (for time series with
  period length p) is
  \deqn{\hat Y[t+h] = a[t] + h b[t] + s[t - p + 1 + (h - 1) \bmod p],}{
    Yhat[t+h] = a[t] + h * b[t] + s[t - p + 1 + (h - 1) mod p],}
  where \eqn{a[t]}, \eqn{b[t]} and \eqn{s[t]} are given by
  \deqn{a[t] = \alpha (Y[t] - s[t-p])  + (1-\alpha) (a[t-1] + b[t-1])}{
  a[t] = \alpha (Y[t] - s[t-p])  + (1-\alpha) (a[t-1] + b[t-1])}
  \deqn{b[t] = \beta (a[t] -a[t-1]) + (1-\beta)  b[t-1]}{
  b[t] = \beta (a[t] - a[t-1]) + (1-\beta) b[t-1]}
  \deqn{s[t] = \gamma (Y[t] - a[t])   + (1-\gamma) s[t-p]}{
  s[t] = \gamma (Y[t] - a[t]) + (1-\gamma) s[t-p]}

  The multiplicative Holt-Winters prediction function (for time series
  with period length p) is
  \deqn{\hat Y[t+h] = (a[t] + h b[t]) \times s[t - p + 1 + (h - 1) \bmod p].}{
    Yhat[t+h] = (a[t] + h * b[t]) * s[t - p + 1 + (h - 1) mod p],}
  where \eqn{a[t]}, \eqn{b[t]} and \eqn{s[t]} are given by
  \deqn{a[t] = \alpha (Y[t] / s[t-p])  + (1-\alpha) (a[t-1] + b[t-1])}{
  a[t] = \alpha (Y[t] / s[t-p])  + (1-\alpha) (a[t-1] + b[t-1])}
  \deqn{b[t] = \beta (a[t] - a[t-1]) + (1-\beta) b[t-1]}{
  b[t] = \beta (a[t] - a[t-1]) + (1-\beta) b[t-1]}
  \deqn{s[t] = \gamma (Y[t] / a[t])   + (1-\gamma) s[t-p]}{
  s[t] = \gamma  (Y[t] / a[t])  + (1-\gamma) s[t-p]}
  The data in \code{x} are required to be non-zero for a multiplicative
  model, but it makes most sense if they are all positive.

  The function tries to find the optimal values of \eqn{\alpha} and/or
  \eqn{\beta} and/or \eqn{\gamma} by minimizing the squared one-step
  prediction error if they are \code{NULL} (the default). \code{optimize}
  will be used for the single-parameter case, and \code{optim} otherwise.

  For seasonal models, start values for \code{a}, \code{b} and \code{s}
  are inferred by performing a simple decomposition in trend and
  seasonal component using moving averages (see function
  \code{\link{decompose}}) on the \code{start.periods} first periods (a simple
  linear regression on the trend component is used for starting level
  and trend). For level/trend-models (no seasonal component), start
  values for \code{a} and \code{b} are \code{x[2]} and \code{x[2] -
  x[1]}, respectively. For level-only models (ordinary exponential
  smoothing), the start value for \code{a} is \code{x[1]}.
}
\value{
  An object of class \code{"HoltWinters"}, a list with components:
  \item{fitted}{A multiple time series with one column for the
    filtered series as well as for the level, trend and seasonal
    components, estimated contemporaneously (that is at time t and not
    at the end of the series).}
  \item{x}{The original series}
  \item{alpha}{alpha used for filtering}
  \item{beta}{beta used for filtering}
  \item{gamma}{gamma used for filtering}
  \item{coefficients}{A vector with named components \code{a, b, s1, ..., sp}
    containing the estimated values for the level, trend and seasonal
    components}
  \item{seasonal}{The specified \code{seasonal} parameter}
  \item{SSE}{The final sum of squared errors achieved in optimizing}
  \item{call}{The call used}
}
\references{
  C. C. Holt (1957)
  Forecasting seasonals and trends by exponentially weighted moving averages,
  \emph{ONR Research Memorandum, Carnegie Institute of Technology} \bold{52}.
  (reprint at \url{https://doi.org/10.1016/j.ijforecast.2003.09.015}).

  P. R. Winters (1960).
  Forecasting sales by exponentially weighted moving averages.
  \emph{Management Science}, \bold{6}, 324--342.
  \doi{10.1287/mnsc.6.3.324}.
}
\author{
  David Meyer \email{David.Meyer@wu.ac.at}
}
\seealso{
  \code{\link{predict.HoltWinters}}, \code{\link{optim}}.
}

% Differences seen on 32-bit Linux at -O3
\examples{
\dontshow{od <- options(digits = 5)}
require(graphics)

## Seasonal Holt-Winters
(m <- HoltWinters(co2))
plot(m)
plot(fitted(m))

(m <- HoltWinters(AirPassengers, seasonal = "mult"))
plot(m)

## Non-Seasonal Holt-Winters
x <- uspop + rnorm(uspop, sd = 5)
m <- HoltWinters(x, gamma = FALSE)
plot(m)

## Exponential Smoothing
m2 <- HoltWinters(x, gamma = FALSE, beta = FALSE)
lines(fitted(m2)[,1], col = 3)
\dontshow{options(od)}
}
\keyword{ts}
