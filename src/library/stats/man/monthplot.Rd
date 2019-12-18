% File src/library/stats/man/monthplot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{monthplot}
\alias{monthplot}
\alias{monthplot.default}
\alias{monthplot.ts}
\alias{monthplot.stl}
\alias{monthplot.StructTS}

\title{
  Plot a Seasonal or other Subseries from a Time Series
}
\description{
  These functions plot seasonal (or other) subseries of a time series.
  For each season (or other category), a time series is plotted.
}
\usage{
monthplot(x, \dots)

\method{monthplot}{stl}(x, labels = NULL, ylab = choice, choice = "seasonal",
          \dots)

\method{monthplot}{StructTS}(x, labels = NULL, ylab = choice, choice = "sea", \dots)

\method{monthplot}{ts}(x, labels = NULL, times = time(x), phase = cycle(x),
             ylab = deparse1(substitute(x)), \dots)

\method{monthplot}{default}(x, labels = 1L:12L,
          ylab = deparse1(substitute(x)),
          times = seq_along(x),
          phase = (times - 1L)\%\%length(labels) + 1L, base = mean,
          axes = TRUE, type = c("l", "h"), box = TRUE,
          add = FALSE,
          col = par("col"), lty = par("lty"), lwd = par("lwd"),
          col.base = col, lty.base = lty, lwd.base = lwd, ...)
}
\arguments{
  \item{x}{Time series or related object.}
  \item{labels}{Labels to use for each \sQuote{season}.}
  \item{ylab}{y label.}
  \item{times}{Time of each observation.}
  \item{phase}{Indicator for each \sQuote{season}.}
  \item{base}{Function to use for reference line for subseries.}
  \item{choice}{Which series of an \code{stl} or \code{StructTS} object?}
  \item{\dots}{Arguments to be passed to the default method or
    graphical parameters.}
  \item{axes}{Should axes be drawn (ignored if \code{add = TRUE})?}
  \item{type}{Type of plot.  The default is to join the points with
    lines, and \code{"h"} is for histogram-like vertical lines.}
  \item{box}{Should a box be drawn (ignored if \code{add = TRUE})?}
  \item{add}{Should thus just add on an existing plot.}
  \item{col, lty, lwd}{Graphics parameters for the series.}
  \item{col.base, lty.base, lwd.base}{Graphics parameters for the
    segments used for the reference lines.}
}
\details{
  These functions extract subseries from a time series and plot them
  all in one frame.  The \code{\link{ts}}, \code{\link{stl}}, and
  \code{\link{StructTS}} methods use the internally recorded frequency and
  start and finish times to set the scale and the seasons.  The default
  method assumes observations come in groups of 12 (though this can be
  changed).

  If the \code{labels} are not given but the \code{phase} is given, then
  the \code{labels} default to the unique values of the \code{phase}.  If
  both are given, then the \code{phase} values are assumed to be indices
  into the \code{labels} array, i.e., they should be in the range
  from 1 to \code{length(labels)}.
}
\value{
  These functions are executed for their side effect of
  drawing a seasonal subseries plot on the current graphical
  window.
}
\author{Duncan Murdoch}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{ts}}, \code{\link{stl}}, \code{\link{StructTS}}
}

\examples{
require(graphics)

## The CO2 data
fit <- stl(log(co2), s.window = 20, t.window = 20)
plot(fit)
op <- par(mfrow = c(2,2))
monthplot(co2, ylab = "data", cex.axis = 0.8)
monthplot(fit, choice = "seasonal", cex.axis = 0.8)
monthplot(fit, choice = "trend", cex.axis = 0.8)
monthplot(fit, choice = "remainder", type = "h", cex.axis = 0.8)
par(op)

## The CO2 data, grouped quarterly
quarter <- (cycle(co2) - 1) \%/\% 3
monthplot(co2, phase = quarter)

## see also JohnsonJohnson
}
\keyword{ hplot }
\keyword{ ts }
