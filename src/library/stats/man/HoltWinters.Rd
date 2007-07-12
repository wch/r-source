\name{HoltWinters}
\alias{HoltWinters}
\title{Holt-Winters Filtering}
\description{
  Computes Holt-Winters Filtering of a given time series.
  Unknown parameters are determined by minimizing the squared
  prediction error.
}
\usage{
HoltWinters(x, alpha = NULL, beta = NULL, gamma = NULL,
            seasonal = c("additive", "multiplicative"),
            start.periods = 3, l.start = NULL, b.start = NULL,
            s.start = NULL,
            optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
            optim.control = list())
}
\arguments{
  \item{x}{An object of class \code{ts}}
  \item{alpha}{\eqn{alpha} parameter of Holt-Winters Filter}
  \item{beta}{\eqn{beta} parameter of Holt-Winters Filter. If set to 0,
    the function will do exponential smoothing.}
  \item{gamma}{\eqn{gamma} parameter used for the seasonal component.
    If set to 0, an non-seasonal model is fitted.}
  \item{seasonal}{Character string to select an \code{"additive"} 
    (the default) or \code{"multiplicative"} seasonal model. The first
    few characters are sufficient. (Only takes effect if 
    \code{gamma} is non-zero).}
  \item{start.periods}{Start periods used in the autodetection of start
    values. Must be at least 3.}
  \item{l.start}{Start value for level (a[0]).}
  \item{b.start}{Start value for trend (b[0]).}
  \item{s.start}{Vector of start values for the seasonal component
    (\eqn{s_1[0]...s_p[0]}{s_1[0]...s_p[0]})}
  \item{optim.start}{Vector with named components \code{alpha},
    \code{beta}, and \code{gamma} containing the starting values for the
    optimizer. Only the values needed must be specified.}
  \item{optim.control}{Optional list with additional control parameters
    passed to \code{optim}.}
}
\details{
  The additive Holt-Winters prediction function (for time series with
  period length p) is
  \deqn{\hat Y[t+h] = a[t] + h b[t] + s[t + 1 + (h - 1) \bmod p],}{
    Yhat[t+h] = a[t] + h * b[t] + s[t + 1 + (h - 1) mod p],}
  where \eqn{a[t]}, \eqn{b[t]} and \eqn{s[t]} are given by
  \deqn{a[t] = \alpha (Y[t] - s[t-p])  + (1-\alpha) (a[t-1] + b[t-1])}{
  a[t] = \alpha (Y[t] - s[t-p])  + (1-\alpha) (a[t-1] + b[t-1])}
  \deqn{b[t] = \beta (a[t] -a[t-1]) + (1-\beta)  b[t-1]}{
  b[t] = \beta (a[t] - a[t-1]) + (1-\beta) b[t-1]}
  \deqn{s[t] = \gamma (Y[t] - a[t])   + (1-\gamma) s[t-p]}{
  s[t] = \gamma (Y[t] - a[t]) + (1-\gamma) s[t-p]}

  The multiplicative Holt-Winters prediction function (for time series
  with period length p) is
  \deqn{\hat Y[t+h] = (a[t] + h b[t]) \times s[t + 1 + (h - 1) \bmod p].}{
    Yhat[t+h] = (a[t] + h * b[t]) * s[t + 1 + (h - 1) mod p],}
  where \eqn{a[t]}, \eqn{b[t]} and \eqn{s[t]} are given by
  \deqn{a[t] = \alpha (Y[t] / s[t-p])  + (1-\alpha) (a[t-1] + b[t-1])}{
  a[t] = \alpha (Y[t] / s[t-p])  + (1-\alpha) (a[t-1] + b[t-1])}
  \deqn{b[t] = \beta (a[t] - a[t-1]) + (1-\beta) b[t-1]}{
  b[t] = \beta (a[t] - a[t-1]) + (1-\beta) b[t-1]}
  \deqn{s[t] = \gamma (Y[t] / a[t])   + (1-\gamma) s[t-p]}{
  s[t] = \gamma  (Y[t] / a[t])  + (1-\gamma) s[t-p]}

  The function tries to find the optimal values of \eqn{\alpha} and/or
  \eqn{\beta} and/or \eqn{\gamma} by minimizing the squared
  one-step prediction error if they are omitted.

  For seasonal models, start values for \code{a}, \code{b} and \code{s} are detected by
  performing a simple decomposition in trend and seasonal component using
  moving averages (see function \code{decompose}) on the
  \code{start.periods} first periods (a simple linear regression on the
  trend component is used for starting level and trend.). For
  level/trend-models (no seasonal component), start values for a and b
  are x[2] and x[2] - x[1], respectively. For level-only models
  (ordinary exponential smoothing), the start value for a is x[1].
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
  \item(gamma){gamma used for filtering}
  \item{coefficients}{A vector with named components \code{a, b, s1, ..., sp}
    containing the estimated values for the level, trend and seasonal
    components}
  \item{seasonal}{The specified \code{seasonal}-parameter}
  \item{SSE}{The final sum of squared errors achieved in optimizing}
  \item{call}{The call used}
}
\references{
  C. C. Holt (1957)
  Forecasting seasonals and trends by exponentially weighted
  moving averages,
  ONR Research Memorandum, Carnigie Institute 52.

  P. R. Winters (1960)
  Forecasting sales by exponentially weighted moving averages,
  \emph{Management Science} \bold{6}, 324--342.
}
\author{
  David Meyer \email{David.Meyer@wu-wien.ac.at}
}
\seealso{
  \code{\link{predict.HoltWinters}},\code{\link{optim}}
}

\examples{
require(graphics)

## Seasonal Holt-Winters
(m <- HoltWinters(co2))
plot(m)
plot(fitted(m))

(m <- HoltWinters(AirPassengers, seasonal = "mult"))
plot(m)

## Non-Seasonal Holt-Winters
x <- uspop + rnorm(uspop, sd = 5)
m <- HoltWinters(x, gamma = 0)
plot(m)

## Exponential Smoothing
m2 <- HoltWinters(x, gamma = 0, beta = 0)
lines(fitted(m2)[,1], col = 3)
}
\keyword{ts}
