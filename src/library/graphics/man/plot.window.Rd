% File src/library/graphics/man/plot.window.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{plot.window}
\alias{plot.window}
\alias{xlim}
\alias{ylim}
\alias{asp}
\concept{aspect ratio}
\concept{log scale}
\concept{axis range}
\title{Set up World Coordinates for Graphics Window}
\description{
  This function sets up the world coordinate system for a graphics
  window.  It is called by higher level functions such as
  \code{\link{plot.default}} (\emph{after} \code{\link{plot.new}}).
}
\usage{
plot.window(xlim, ylim, log = "", asp = NA, \dots)
}
\arguments{
  \item{xlim, ylim}{numeric vectors of length 2, giving the x and y
    coordinates ranges.}
  \item{log}{character; indicating which axes should be in log scale.}
  \item{asp}{numeric, giving the \bold{asp}ect ratio y/x,
    see \sQuote{Details}.}
  \item{\dots}{further \link{graphical parameters} as in \code{\link{par}}.
    The relevant ones are \code{xaxs}, \code{yaxs} and \code{lab}.
  }
}
\details{
  \describe{
    \item{asp:}{
      If \code{asp} is a finite positive value then the window is
      set up so that one data unit in the x direction is equal in length to
      \code{asp} \eqn{\times}{*} one data unit in the y direction.

      Note that in this case, \code{\link{par}("usr")} is no longer
      determined by, e.g., \code{par("xaxs")}, but rather by \code{asp} and
      the device's aspect ratio. (See what happens if you interactively
      resize the plot device after running the example below!)

      The special case \code{asp == 1} produces plots where distances
      between points are represented accurately on screen.  Values with
      \code{asp > 1} can be used to produce more accurate maps when using
      latitude and longitude.
  }}

  Note that the coordinate ranges will be extended by 4\% if the
  appropriate \link{graphical parameter} \code{xaxs} or \code{yaxs} has value
  \code{"s"} (which is the default).

  To reverse an axis, use \code{xlim} or \code{ylim} of the form
  \code{c(hi, lo)}.

  The function attempts to produce a plausible set of scales if one or
  both of \code{xlim} and \code{ylim} is of length one or the two values
  given are identical, but it is better to avoid that case.

  Usually, one should rather use the higher-level functions such as
  \code{\link{plot}}, \code{\link{hist}}, \code{\link{image}}, \dots,
  instead and refer to their help pages for explanation of the
  arguments.

  A side-effect of the call is to set up the \code{usr}, \code{xaxp} and
  \code{yaxp} \link{graphical parameters}.  (It is for the latter two that
  \code{lab} is used.)
}
\seealso{
  \code{\link{xy.coords}},
  \code{\link{plot.xy}},
  \code{\link{plot.default}}.

  \code{\link{par}} for the graphical parameters mentioned.
}
\examples{
##--- An example for the use of 'asp' :
require(stats)  # normally loaded
loc <- cmdscale(eurodist)
rx <- range(x <- loc[,1])
ry <- range(y <- -loc[,2])
plot(x, y, type = "n", asp = 1, xlab = "", ylab = "")
abline(h = pretty(rx, 10), v = pretty(ry, 10), col = "lightgray")
text(x, y, labels(eurodist), cex = 0.8)
}
\keyword{aplot}
