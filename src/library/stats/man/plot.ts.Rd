\name{plot.ts}
\alias{plot.ts}
\alias{lines.ts}
\title{Plotting Time-Series Objects}
\description{
  Plotting method for objects inheriting from class \code{"ts"}.
}
\usage{
\method{plot}{ts}(x, y = NULL, plot.type = c("multiple", "single"),
     xy.labels, xy.lines, panel = lines, nc, ...)

\method{lines}{ts}(x, \dots)
}
\arguments{
  \item{x, y}{time series objects, usually inheriting from class \code{"ts"}.}

  \item{plot.type}{for multivariate time series, should the series by
    plotted separately (with a common time axis) or on a single plot?}

  \item{xy.labels}{logical, indicating if \code{\link{text}()} labels
    should be used for an x-y plot, \emph{or} character, supplying a
    vector of labels to be used.  The default is to label for up to 150
    points, and not for more.}

  \item{xy.lines}{logical, indicating if \code{\link{lines}}
    should be drawn for an x-y plot.  Defaults to the value of
    \code{xy.labels} if that is logical, otherwise to \code{TRUE}.}

  \item{panel}{a \code{function(x, col, bg, pch, type, ...)} which gives the
    action to be carried out in each panel of the display for
    \code{plot.type="multiple"}.  The default is \code{lines}.}

  \item{nc}{the number of columns to use when \code{type="multiple"}.
    Defaults to 1 for up to 4 series, otherwise to 2.}

  \item{\dots}{additional graphical arguments, see \code{\link{plot}},
    \code{\link{plot.default}} and \code{\link{par}}.}
}
\details{
  If \code{y} is missing, this function creates a time series
  plot, for multivariate series of one of two kinds depending on
  \code{plot.type}.

  If \code{y} is present, both \code{x} and \code{y} must be univariate,
  and a \dQuote{scatter} plot \code{y ~ x} will be drawn, enhanced by
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
plot(z, type = "b")    # multiple
plot(z, plot.type="single", lty=1:3, col=4:2)

## A phase plot:
data(nhtemp)
plot(nhtemp, c(nhtemp[-1], NA), cex = .8, col="blue",
     main = "Lag plot of New Haven temperatures")
## a clearer way to do this would be
\dontrun{
plot(nhtemp, lag(nhtemp, 1), cex = .8, col="blue",
     main = "Lag plot of New Haven temperatures")
}

data(sunspots)
## xy.lines and xy.labels are FALSE for large series:
plot(lag(sunspots, 1), sunspots, pch = ".")

data(EuStockMarkets)
SMI <- EuStockMarkets[, "SMI"]
plot(lag(SMI,  1), SMI, pch = ".")
plot(lag(SMI, 20), SMI, pch = ".", log = "xy",
     main = "4 weeks lagged SMI stocks -- log scale", xy.lines= TRUE)
}
\keyword{hplot}
\keyword{ts}
