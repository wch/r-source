\name{acf}
\alias{acf}
\alias{ccf}
\alias{pacf}
\alias{pacf.default}
\alias{pacf.ts}
\alias{pacf.mts}
\alias{plot.acf}
\title{Autocovariance and Autocorrelation Function Estimation}
\usage{
acf(x, lag.max = NULL,
    type = c("correlation", "covariance", "partial"),
    plot = TRUE, na.action, demean = TRUE, ...)
pacf(x, lag.max = NULL, plot = TRUE, na.action, ...)
ccf(x, y, lag.max = NULL, type = c("correlation", "covariance"),
    plot = TRUE,na.action, ...)

plot.acf(acf.obj, ci=0.95, ci.col="blue", ci.type=c("white", "ma"), ...)
}
\arguments{
    \item{x, y}{a univariate or multivariate (not \code{ccf}) time
	series object or a numeric vector or matrix.}

    \item{lag.max}{maximum lag at which to calculate the acf.  Default
	is \eqn{10\log_{10}(N)}{10*log10(N)} where \eqn{N} is the number
	of observations.}

    \item{plot}{logical. If \code{TRUE} the acf is plotted.}

    \item{type}{character string giving type of acf.  Allowed values are
 	"correlation" (the default), "covariance" or "partial".}

    \item{na.action}{function to be called to handle missing values.}

    \item{demean}{logical. Should the covariances be about the sample means?}

    \item{acf.obj}{an object of class \code{acf}.}

    \item{ci}{coverage probability for confidence interval.  Plotting of
	the confidence interval is suppressed if \code{ci} is
	zero or negative.}

    \item{ci.col}{colour to plot the confidence interval lines.}

    \item{ci.type}{should the confidence limits assume a white noise
	input or for lag \code{k} an MA(\code{k-1}) input?}

    \item{\dots}{graphical parameters.}
}
\description{
    The function \code{acf} computes (and by default plots) estimates of
    the autocovariance or autocorrelation function.  Function
    \code{pacf} is the function used for the partial autocorrelations.

    Function \code{ccf} computes the cross-correlation or
    cross-covariance of two univariate series.

    The generic function \code{plot} has a method for \code{acf} objects.
}
\details{
    For \code{type} = \code{"correlation"} and \code{"covariance"}, the
    estimates are based on the sample covariance.
    
    The partial correlation coefficient is estimated by fitting
    autoregressive models of successively higher orders up to
    \code{lag.max}.
}
\value{
  An object of class \code{acf}, which is a list with the following
  elements:

  \item{lag}{A three dimensional array containing the lags at which
      the acf is estimated.}

  \item{acf}{An array with the same dimensions as \code{lag} 
      containing the estimated acf.}

  \item{type}{The type of correlation (same as the \code{type} argument).}

  \item{n.used}{The number of observations in the time series.}

  \item{series}{The name of the series \code{x}.}

  \item{snames}{The series names for a multivariate time series.}

  The result is returned invisibly if \code{plot} is \code{TRUE}.
}
\author{Original: Paul Gilbert, Martyn Plummer.  Extensive modifications
    and univariate case of \code{pacf} by B.D. Ripley.}

\note{The confidence interval plotted in \code{plot.acf} is based on an
    \emph{uncorrelated} series and should be treated with appropriate
    caution. Using \code{ci.type = "ma"} may be less potentially misleading.}

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
