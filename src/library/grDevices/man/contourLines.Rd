\name{contourLines}
\alias{contourLines}
\title{Calculate Contour Lines}
\description{
  Calculate contour lines for a given set of data.
}
\usage{
contourLines(x = seq(0, 1, length.out = nrow(z)),
             y = seq(0, 1, length.out = ncol(z)),
	     z, nlevels = 10,
             levels = pretty(range(z, na.rm=TRUE), nlevels))
}
\arguments{
  \item{x,y}{locations of grid lines at which the values in \code{z} are
    measured.  These must be in ascending order.  By default, equally
    spaced values from 0 to 1 are used.  If \code{x} is a \code{list},
    its components \code{x$x} and \code{x$y} are used for \code{x}
    and \code{y}, respectively. If the list has component \code{z} this
    is used for \code{z}.}
  \item{z}{a matrix containing the values to be plotted (\code{NA}s are
    allowed).  Note that \code{x} can be used instead of \code{z} for
    convenience.}
  \item{nlevels}{number of contour levels desired \bold{iff}
    \code{levels} is not supplied.}
  \item{levels}{numeric vector of levels at which to draw contour
    lines.}
}
\details{
  \code{contourLines} draws nothing, but returns a set of contour lines.
  
  There is currently no documentation about the algorithm.
  The source code is in \file{\$R\_HOME/src/main/plot3d.c}.
}
\value{
  A list of contours.
  Each contour is a list with elements:
  \item{level }{The contour level.}
  \item{x }{The x-coordinates of the contour.}
  \item{y }{The y-coordinates of the contour.}
}
\seealso{
  \code{\link{options}("max.countour.segments")} for the maximal
  complexity of a single contour line.

  \code{\link[graphics]{contour}}.
}
\examples{
x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
contourLines(x, y, volcano)
}
\keyword{dplot}
