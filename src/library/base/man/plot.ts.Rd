\name{plot.ts}
\alias{plot.ts}
\alias{plot.mts}
\alias{lines.ts}
\title{Plotting Time-Series Objects}
\description{
  Plotting methods for objects of class \code{"ts"} or \code{"mts"}
  (multivariate time-series).
}
\usage{
\method{plot}{ts}(x, y = NULL, type = "l", xlim = NULL, ylim = NULL,
     xlab = "Time", ylab, log = "",
     col = par("col"), bg = NA, pch = par("pch"), cex = par("cex"),
     lty = par("lty"), lwd = par("lwd"),
     axes = TRUE, frame.plot = axes, ann = par("ann"),
     main = NULL, plot.type = c("multiple", "single"),
     xy.labels = n <= 150, xy.lines = do.lab, ...)

lines(x, \dots)
}
\arguments{
  \item{x,y}{time series objects, usually of class \code{"ts"}.}

  \item{type}{the type of plot, see \code{\link{plot}}.  When \code{y}
    is present, the default will depend on \code{xy.labels}, see below.}

  \item{plot.type}{for multivariate time series, should the series by
    plotted separately (with a common time axis) or on a single plot?}

  \item{xy.labels}{logical, indicating if \code{\link{text}()} labels
    should be used for an x-y plot.}
  \item{xy.lines}{logical, indicating if \code{\link{lines}}
    should be drawn for an x-y plot.  Default is true, when labels are
    drawn as well.}

  \item{\dots}{additional graphical arguments, see \code{\link{plot}},
    \code{\link{plot.default}} and \code{\link{par}}.}
}
\details{
  With one prinicipal argument, these functions create time series
  plots, for multivariate series of two kinds depending on \code{plot.type},

  If \code{y} is present, both \code{x} and \code{y} must be univariate,
  and a ``scatter'' plot \code{y ~ x} will be drawn, enhanced by
  using \code{\link{text}} if \code{xy.labels} is
  \code{\link{TRUE}} or \code{character}, and \code{\link{lines}} if
  \code{xy.lines} is \code{TRUE}.
}
\seealso{
  \code{\link{ts}} for basic time series construction and access
  functionality.
}
\examples{

## Multivariate
z <- ts(matrix(rt(300, df = 3), 100, 3), start=c(1961, 1), frequency=12)
plot(z)# multiple
plot(z, plot.type="single", lty=1:3)

## A phase plot:
data(nhtemp)
plot(nhtemp, c(nhtemp[-1], NA), cex = .8, col="blue",
     main = "Lag plot of New Haven temperatures")
## a clearer way to do this would be
\dontrun{library(ts)
plot(nhtemp, lag(nhtemp, 1), cex = .8, col="blue",
     main = "Lag plot of New Haven temperatures")
}

library(ts)

data(sunspots)
## xy.lines and xy.labels are FALSE for large series:
plot(lag(sunspots, 1), sunspots, pch = ".")

data(EuStockMarkets)
SMI <- EuStockMarkets[, "SMI"]
plot(lag(SMI,  1), SMI, pch = ".")
plot(lag(SMI, 20), SMI, pch = ".", log = "xy",
     main = "4 weeks lagged SMI stocks -- log scale", xy.lines= TRUE)

detach("package:ts") 
}
\keyword{hplot}
\keyword{ts}
