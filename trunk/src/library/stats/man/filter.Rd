\name{filter}
\alias{filter}
\title{Linear Filtering on a Time Series}
\usage{
filter(x, filter, method = c("convolution", "recursive"),
       sides = 2, circular = FALSE, init)
}
\arguments{
  \item{x}{a univariate or multivariate time series.}

  \item{filter}{a vector of filter coefficients in reverse time order
    (as for AR or MA coefficients).}

  \item{method}{Either \code{"convolution"} or \code{"recursive"} (and
    can be abbreviated). If \code{"convolution"} a moving average is
    used: if \code{"recursive"} an autoregression is used.}

  \item{sides}{for convolution filters only. If \code{sides=1} the
    filter coefficients are for past values only; if \code{sides=2}
    they are centred around lag 0. In this case the length of the
    filter should be odd, but if it is even, more of the filter
    is forward in time than backward.}

  \item{circular}{for convolution filters only.  If \code{TRUE}, wrap
    the filter around the ends of the series, otherwise assume
    external values are missing (\code{NA}).}

  \item{init}{for recursive filters only. Specifies the initial values
    of the time series just prior to the start value, in reverse
    time order. The default is a set of zeros.}
}
\description{
  Applies linear filtering to a univariate time series or to each series
  separately of a multivariate time series.
}
\value{
  A time series object.
}
\details{
  Missing values are allowed in \code{x} but not in \code{filter}
  (where they would lead to missing values everywhere in the output).

  Note that there is an implied coefficient 1 at lag 0 in the
  recursive filter, which gives
  \deqn{y_i = x_i + f_1y_{i-1} + \cdots + f_py_{i-p}}{\code{y[i] =
      x[i] + f[1]*y[i-1] + \dots + f[p]*y[i-p]}}
  No check is made to see if recursive filter is invertible:
  the output may diverge if it is not.

  The convolution filter is
  \deqn{y_i = f_1x_{i+o} + \cdots + f_px_{i+o-(p-1)}}{\code{y[i] = f[1]*x[i+o] + \dots + f[p]*x[i+o-(p-1)]}}

  where \code{o} is the offset: see \code{sides} for how it is determined.
}
\note{
  \code{\link{convolve}(, type="filter")} uses the FFT for computations
  and so \emph{may} be faster for long filters on univariate series,
  but it does not return a time series (and so the  time alignment is
  unclear), nor does it handle missing values.  \code{filter} is
  faster for a filter of length 100 on a series of length 1000,
  for example.
}

\seealso{
  \code{\link{convolve}}, \code{\link{arima.sim}}
}

\examples{
x <- 1:100
filter(x, rep(1, 3))
filter(x, rep(1, 3), sides = 1)
filter(x, rep(1, 3), sides = 1, circular = TRUE)

filter(presidents, rep(1,3))
}
\keyword{ts}
