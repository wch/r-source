% File src/library/stats/man/lag.plot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{lag.plot}
\alias{lag.plot}
\title{Time Series Lag Plots}
\description{
  Plot time series against lagged versions of themselves.
  Helps visualizing \sQuote{auto-dependence} even when auto-correlations
  vanish.
}
\usage{
lag.plot(x, lags = 1, layout = NULL, set.lags = 1:lags,
         main = NULL, asp = 1,
         diag = TRUE, diag.col = "gray", type = "p", oma = NULL,
         ask = NULL, do.lines = (n <= 150), labels = do.lines,
         \dots)
}
\arguments{
  \item{x}{time-series (univariate or multivariate)}
  \item{lags}{number of lag plots desired, see arg \code{set.lags}.}
  \item{layout}{the layout of multiple plots, basically the \code{mfrow}
    \code{\link{par}()} argument.  The default uses about a square
    layout (see \code{\link{n2mfrow}} such that all plots are on one page.}
  \item{set.lags}{vector of positive integers allowing specification of
    the set of lags used; defaults to \code{1:lags}.}
  \item{main}{character with a main header title to be done on the top
    of each page.}
  \item{asp}{Aspect ratio to be fixed, see \code{\link{plot.default}}.}
  \item{diag}{logical indicating if the x=y diagonal should be drawn.}
  \item{diag.col}{color to be used for the diagonal \code{if(diag)}.}
  \item{type}{plot type to be used, but see \code{\link{plot.ts}} about
    its restricted meaning.}
  \item{oma}{outer margins, see \code{\link{par}}.}
  \item{ask}{logical or \code{NULL}; if true, the user is asked to
    confirm before a new page is started.}
  \item{do.lines}{logical indicating if lines should be drawn.}
  \item{labels}{logical indicating if labels should be used.}
  \item{\dots}{Further arguments to \code{\link{plot.ts}}.  Several
    graphical parameters are set in this function and so cannot be
    changed: these include \code{xlab}, \code{ylab}, \code{mgp},
    \code{col.lab} and \code{font.lab}: this also applies to the
    arguments \code{xy.labels} and \code{xy.lines}.}
}
\details{
  If just one plot is produced, this is a conventional plot.  If more
  than one plot is to be produced, \code{par(mfrow)} and several other
  graphics parameters will be set, so it is not (easily) possible to mix
  such lag plots with other plots on the same page.

  If \code{ask = NULL}, \code{par(ask = TRUE)} will be called if more than
  one page of plots is to be produced and the device is interactive.
}
\note{
  It is more flexible and has different default behaviour than
  the S version.  We use \code{main =} instead of \code{head = } for
  internal consistency.
}
\author{Martin Maechler}
\seealso{
  \code{\link{plot.ts}} which is the basic work horse.
}
\examples{
require(graphics)

lag.plot(nhtemp, 8, diag.col = "forest green")
lag.plot(nhtemp, 5, main = "Average Temperatures in New Haven")
## ask defaults to TRUE when we have more than one page:
lag.plot(nhtemp, 6, layout = c(2,1), asp = NA,
         main = "New Haven Temperatures", col.main = "blue")

## Multivariate (but non-stationary! ...)
lag.plot(freeny.x, lags = 3)

## no lines for long series :
lag.plot(sqrt(sunspots), set = c(1:4, 9:12), pch = ".", col = "gold")
}
\keyword{hplot}
\keyword{ts}
