\name{stl}
\alias{stl}
\alias{print.stl}
\alias{plot.stl}
\title{Seasonal Decomposition of Time Series by Loess}
\usage{
stl(x, s.window = NULL, s.degree = 0, t.window = NULL, t.degree = 1,
    robust = FALSE, na.action = na.fail)
}
\arguments{
    \item{x}{A univariate time series to be decomposed.
	This should be an object of class \code{"ts"} with a frequency
	greater than one.}
    \item{s.window}{Either the string \code{"periodic"} or the span (in
	lags) of the loess window for seasonal extraction, which should
	be odd.  This has no default.}
    \item{s.degree}{Degree of locally-fitted polynomial in seasonal
	extraction.  Should be zero or one.} 
    \item{t.window}{The span (in lags) of the loess window for trend
	extraction, which should be odd.  There is a reasonable
	default.}
    \item{t.degree}{Degree of locally-fitted polynomial in trend
	extraction.  Should be zero or one.}
    \item{robust}{Should robust fitting be used in the \code{loess}
	procedure?}
    \item{na.action}{Action on missing values.}
}
\description{
    Decompose a time series into seasonal, trend and irregular
    components.
}
\details{
    The seasonal component is found by \emph{loess} smoothing the
    seasonal sub-series (the series of all January values, \ldots); if
    \code{s.window = "periodic"} smoothing is effectively replaced by
    taking the mean. The seasonal values are removed, and the remainder
    smoothed to find the trend. The overall level is removed from the
    seasonal component and added to the trend component. This process is
    iterated a few times.  The \code{remainder} component is the
    residuals from the seasonal plus trend fit.
}
\value{
    An object of class \code{"stl"} with components
    \item{time.series}{a multiple time series with columns
      	\code{seasonal}, \code{trend} and \code{remainder},}
    \item{weights}{the final robust weights (all one if fitting is not
      	done robustly,}
    \item{call}{the matched call.}
}
\references{
     R. B. Cleveland, W. S. Cleveland, J.E.  McRae, and I.  Terpenning
     (1990).
     STL:  A  Seasonal-Trend  Decomposition  Procedure Based on Loess.
     \emph{Journal of Official Statistics}, \bold{6}, 3-73.
}
\author{B.D. Ripley; Fortran code by Cleveland \emph{et al.} (1990) from
    \file{netlib}.}

\note{This is similar to but not identical to the \code{stl} function in
    S-PLUS. The \code{remainder} componet given by S-PLUS is the sum of
    the \code{trend} and \code{remainder} series from this function.}

\seealso{\code{\link{loess}} in package \code{modreg} (which is not
    actually used in \code{stl}).}

\examples{
data(nottem)
plot(stl(nottem, "per"))
data(co2)
plot(stl(log(co2), s.window=21))
## linear trend, strict period.
plot(stl(log(co2), s.window="per", t.window=1000))
}
\keyword{ts}
