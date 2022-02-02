% File src/library/stats/man/acf.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{acf}
\alias{acf}
\alias{ccf}
\alias{pacf}
\alias{pacf.default}
\alias{[.acf}
\title{Auto- and Cross- Covariance and -Correlation Function Estimation}
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
    plot = TRUE, na.action = na.fail, demean = TRUE, \dots)

pacf(x, lag.max, plot, na.action, \dots)

\method{pacf}{default}(x, lag.max = NULL, plot = TRUE, na.action = na.fail,
    ...)

ccf(x, y, lag.max = NULL, type = c("correlation", "covariance"),
    plot = TRUE, na.action = na.fail, \dots)

\method{[}{acf}(x, i, j)
}
\arguments{
  \item{x, y}{a univariate or multivariate (not \code{ccf}) numeric time
    series object or a numeric vector or matrix, or an \code{"acf"} object.}
  \item{lag.max}{maximum lag at which to calculate the acf.
    Default is \eqn{10\log_{10}(N/m)}{10*log10(N/m)} where \eqn{N} is the
    number of observations and \eqn{m} the number of series.  Will
    be automatically limited to one less than the number of observations
    in the series.}
  \item{type}{character string giving the type of acf to be computed.
    Allowed values are
    \code{"correlation"} (the default), \code{"covariance"} or
    \code{"partial"}.  Will be partially matched.}
  \item{plot}{logical. If \code{TRUE} (the default) the acf is plotted.}
  \item{na.action}{function to be called to handle missing
    values. \code{na.pass} can be used.}
  \item{demean}{logical.  Should the covariances be about the sample
    means?}
  \item{\dots}{further arguments to be passed to \code{plot.acf}.}

  \item{i}{a set of lags (time differences) to retain.}
  \item{j}{a set of series (names or numbers) to retain.}
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

  The lag \code{k} value returned by \code{ccf(x, y)} estimates the
  correlation between \code{x[t+k]} and \code{y[t]}.

  The result is returned invisibly if \code{plot} is \code{TRUE}.
}
\details{
  For \code{type} = \code{"correlation"} and \code{"covariance"}, the
  estimates are based on the sample covariance. (The lag 0 autocorrelation
  is fixed at 1 by convention.)

  By default, no missing values are allowed.  If the \code{na.action}
  function passes through missing values (as \code{na.pass} does), the
  covariances are computed from the complete cases.  This means that the
  estimate computed may well not be a valid autocorrelation sequence,
  and may contain missing values.  Missing values are not allowed when
  computing the PACF of a multivariate time series.

  The partial correlation coefficient is estimated by fitting
  autoregressive models of successively higher orders up to
  \code{lag.max}.

  The generic function \code{plot} has a method for objects of class
  \code{"acf"}.

  The lag is returned and plotted in units of time, and not numbers of
  observations.

  There are \code{print} and subsetting methods for objects of class
  \code{"acf"}.
}
\author{
  Original: Paul Gilbert, Martyn Plummer.
  Extensive modifications and univariate case of \code{pacf} by
  B. D. Ripley.
}
\references{
  Venables, W. N. and Ripley, B. D. (2002)
  \emph{Modern Applied Statistics with S}.  Fourth Edition.
  Springer-Verlag.

  (This contains the exact definitions used.)
}

\seealso{
  \code{\link{plot.acf}}, \code{\link{ARMAacf}} for the exact
  autocorrelations of a given ARMA process.
}
\examples{
require(graphics)

## Examples from Venables & Ripley
acf(lh)
acf(lh, type = "covariance")
pacf(lh)

acf(ldeaths)
acf(ldeaths, ci.type = "ma")
acf(ts.union(mdeaths, fdeaths))
ccf(mdeaths, fdeaths, ylab = "cross-correlation")
# (just the cross-correlations)

presidents # contains missing values
acf(presidents, na.action = na.pass)
pacf(presidents, na.action = na.pass)
}
\keyword{ts}
