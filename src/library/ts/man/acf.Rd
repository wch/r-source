\name{acf}
\alias{acf}
\alias{ccf}
\alias{pacf}
\alias{pacf.default}
\alias{pacf.ts}
\alias{pacf.mts}
\title{Autocovariance and Autocorrelation Function Estimation}
\description{
  The function \code{acf} computes (and by default plots) estimates of
  the autocovariance or autocorrelation function.  Function \code{pacf}
  is the function used for the partial autocorrelations.  Function
  \code{ccf} computes the cross-correlation or cross-covariance of two
  univariate series.
}
\usage{
acf(x, lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action, demean = TRUE, \dots)
pacf(x, lag.max = NULL, plot = TRUE, na.action, \dots)
ccf(x, y, lag.max = NULL, type = c("correlation", "covariance"),
    plot = TRUE, na.action, \dots)
}
\arguments{
  \item{x, y}{a univariate or multivariate (not \code{ccf}) time
    series object or a numeric vector or matrix.}
  \item{lag.max}{maximum lag at which to calculate the acf.  Default
    is \eqn{10\log_{10}(N)}{10*log10(N)} where \eqn{N} is the number
    of observations.}
  \item{type}{character string giving the type of acf to be computed.
    Allowed values are
    \code{"correlation"} (the default), \code{"covariance"} or
    \code{"partial"}.}
  \item{plot}{logical. If \code{TRUE} the acf is plotted.}
  \item{na.action}{function to be called to handle missing values.}
  \item{demean}{logical.  Should the covariances be about the sample
    means?}
  \item{\dots}{further arguments to be passed to \code{plot.acf}.}
}
\value{
  An object of class \code{"acf"}, which is a list with the following
  elements:

  \item{lag}{A three dimensional array containing the lags at which
    the acf is estimated.}
  \item{acf}{An array with the same dimensions as \code{lag} containing
    the estimated acf.}
  \item{type}{The type of correlation (same as the \code{type}
    argument).}
  \item{n.used}{The number of observations in the time series.}
  \item{series}{The name of the series \code{x}.}
  \item{snames}{The series names for a multivariate time series.}

  The result is returned invisibly if \code{plot} is \code{TRUE}.
}
\details{
  For \code{type} = \code{"correlation"} and \code{"covariance"}, the
  estimates are based on the sample covariance.

  The partial correlation coefficient is estimated by fitting
  autoregressive models of successively higher orders up to
  \code{lag.max}.

  The generic function \code{plot} has a method for objects of class
  \code{"acf"}.
}
\author{
  Original: Paul Gilbert, Martyn Plummer.
  Extensive modifications and univariate case of \code{pacf} by
  B.D. Ripley.
}
\seealso{
  \code{\link{plot.acf}}
}
\examples{
## Examples from Venables & Ripley
data(lh)
acf(lh)
acf(lh, type="covariance")
pacf(lh)

data(UKLungDeaths)
acf(ldeaths)
acf(ldeaths, ci.type="ma")
acf(ts.union(mdeaths, fdeaths))
ccf(mdeaths, fdeaths) # just the cross-correlations.
}
\keyword{ts}
