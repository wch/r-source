\name{plot.window}
\alias{plot.window}
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
  \item{xlim, ylim}{numeric of length 2, giving the x and y coordinates
    ranges.}
  \item{log}{character; indicating which axes should be in log scale.}
  \item{asp}{numeric, giving the \bold{asp}ect ratio y/x.}
  \item{\dots}{further graphical parameters as in \code{\link{par}}.
    Relevant ones are \code{xaxs}, \code{yaxs} and \code{lab}.
  }
}
\details{
  Note that if \code{asp} is a finite positive value then the window is
  set up so that one data unit in the x direction is equal in length to
  \code{asp} \eqn{\times}{*} one data unit in the y direction.

  The special case \code{asp == 1} produces plots where distances
  between points are represented accurately on screen.  Values with
  \code{asp > 1} can be used to produce more accurate maps when using
  latitude and longitude.

  The function attempts to produce a plausible set of scales if one or
  both of \code{xlim} and \code{ylim} is of length one or the two values
  given are identical, but it is better to avoid that case.

  Usually, one should rather use the higher level functions such as
  \code{\link{plot}}, \code{\link{hist}}, \code{\link{image}}, \dots,
  instead and refer to their help pages for explanation of the
  arguments.

  A side-effect of the call is to set up the \code{usr}, \code{xaxp} and
  \code{yaxp} graphical parameters.  (It is for the latter two that
  \code{lab} is used.)
}
\seealso{
  \code{\link{xy.coords}},
  \code{\link{plot.xy}},
  \code{\link{plot.default}}.
}
\examples{
##--- An example for the use of 'asp' :
require(stats)  # normally loaded
loc <- cmdscale(eurodist)
rx <- range(x <- loc[,1])
ry <- range(y <- -loc[,2])
plot(x, y, type="n", asp=1, xlab="", ylab="")
abline(h = pretty(rx, 10), v = pretty(ry, 10), col = "lightgray")
text(x, y, labels(eurodist), cex=0.8)
}
\keyword{aplot}
