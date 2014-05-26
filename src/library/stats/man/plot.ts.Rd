% File src/library/stats/man/plot.ts.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{plot.ts}
\alias{plot.ts}
\alias{lines.ts}
\title{Plotting Time-Series Objects}
\description{
  Plotting method for objects inheriting from class \code{"ts"}.
}
\usage{
\method{plot}{ts}(x, y = NULL, plot.type = c("multiple", "single"),
        xy.labels, xy.lines, panel = lines, nc, yax.flip = FALSE,
        mar.multi = c(0, 5.1, 0, if(yax.flip) 5.1 else 2.1),
        oma.multi = c(6, 0, 5, 0), axes = TRUE, \dots)

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
    \code{plot.type = "multiple"}.  The default is \code{lines}.}

  \item{nc}{the number of columns to use when \code{type = "multiple"}.
    Defaults to 1 for up to 4 series, otherwise to 2.}
  \item{yax.flip}{logical indicating if the y-axis (ticks and numbering)
    should flip from side 2 (left) to 4 (right) from series to series
    when \code{type = "multiple"}.}
  \item{mar.multi, oma.multi}{the (default) \code{\link{par}} settings
    for \code{plot.type = "multiple"}.  Modify with care!}
  \item{axes}{logical indicating if x- and y- axes should be drawn.}
  \item{\dots}{additional graphical arguments, see \code{\link{plot}},
    \code{\link{plot.default}} and \code{\link{par}}.}
}
\details{
  If \code{y} is missing, this function creates a time series
  plot, for multivariate series of one of two kinds depending on
  \code{plot.type}.

  If \code{y} is present, both \code{x} and \code{y} must be univariate,
  and a scatter plot \code{y ~ x} will be drawn, enhanced by
  using \code{\link{text}} if \code{xy.labels} is
  \code{\link{TRUE}} or \code{character}, and \code{\link{lines}} if
  \code{xy.lines} is \code{TRUE}.
}
\seealso{
  \code{\link{ts}} for basic time series construction and access
  functionality.
}
\examples{
require(graphics)

## Multivariate
z <- ts(matrix(rt(200 * 8, df = 3), 200, 8),
        start = c(1961, 1), frequency = 12)
plot(z, yax.flip = TRUE)
plot(z, axes = FALSE, ann = FALSE, frame.plot = TRUE,
     mar.multi = c(0,0,0,0), oma.multi = c(1,1,5,1))
title("plot(ts(..), axes=FALSE, ann=FALSE, frame.plot=TRUE, mar..., oma...)")

z <- window(z[,1:3], end = c(1969,12))
plot(z, type = "b")    # multiple
plot(z, plot.type = "single", lty = 1:3, col = 4:2)

## A phase plot:
plot(nhtemp, lag(nhtemp, 1), cex = .8, col = "blue",
     main = "Lag plot of New Haven temperatures")

## xy.lines and xy.labels are FALSE for large series:
plot(lag(sunspots, 1), sunspots, pch = ".")

SMI <- EuStockMarkets[, "SMI"]
plot(lag(SMI,  1), SMI, pch = ".")
plot(lag(SMI, 20), SMI, pch = ".", log = "xy",
     main = "4 weeks lagged SMI stocks -- log scale", xy.lines =  TRUE)
}
\keyword{hplot}
\keyword{ts}
