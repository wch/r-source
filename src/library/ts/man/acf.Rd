\name{acf}
\alias{acf}
\alias{pacf}
\alias{pacf.ts}
\alias{pacf.mts}
\alias{plot.acf}
\title{Autocovariance and Autocorrelation Function Estimation}
\usage{
acf(x, lag.max = NULL, plot = TRUE,
    type = c("correlation", "covariance", "partial"),
    na.action, ...)
pacf(x, lag.max = NULL, plot = TRUE, na.action, ...)
plot.acf(acf.obj, ci, ...)
}
\arguments{
 \item{x}{a time series object, univariate or multivariate.}

 \item{lag.max}{maximum lag at which to calculate the acf. Default is
     10*log10(N) where N is the number of observations.}

 \item{plot}{logical. If \code{TRUE} the acf is plotted.}

 \item{type}{character string giving type of acf. Allowed values are
     "correlation" (the default), "covariance" or "partial".}

 \item{na.action}{function to be called to handle missing values.}

 \item{acf.obj}{an object of class \code{acf}}

 \item{ci}{coverage probability for confidence interval. Plotting of the
     confidence interval is suppressed if \code{ci} <= 0.}

 \item{\dots}{graphical parameters.}
}
\description{
    The function \code{acf} computes estimates of the autocovariance /
    autocorrelation function. Function \code{pacf} is the functions used
    for the partial autocorrelations.

    The generic function \code{plot} has a method for acf objects.
}
\details{
    For \code{type} = "correlation" and "covariance", the
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
  \item{snames}{The series names for multivariate time series.}
}
\author{Original: Paul Gilbert, Martyn Plummer.  Modifications and
univaraite case of pacf by B.D. Ripley.}

\note{The confidence interval plotted in \code{plot.acf} is based on
an \emph{uncorrelated} series and should be treated with appropriate
caution.}

%\seealso{\code{\link{ar.yw}}}

\examples{
data(lh)
acf(lh)
acf(lh, type="covariance")
pacf(lh)

data(deaths)
acf(deaths)
acf(ts.union(mdeaths, fdeaths))
}
\keyword{ts}
