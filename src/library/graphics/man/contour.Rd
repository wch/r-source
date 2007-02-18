\name{contour}
\alias{contour}
\alias{contour.default}
\title{Display Contours}
\description{
  Create a contour plot, or add contour lines to an existing plot.
}
\usage{
contour(x, \dots)

\method{contour}{default}(x = seq(0, 1, len = nrow(z)),
        y = seq(0, 1, len = ncol(z)),
        z,
        nlevels = 10, levels = pretty(zlim, nlevels), labels = NULL,
        xlim = range(x, finite = TRUE),
        ylim = range(y, finite = TRUE),
        zlim = range(z, finite = TRUE),
        labcex = 0.6, drawlabels = TRUE, method = "flattest",
	vfont = c("sans serif", "plain"),
        axes = TRUE, frame.plot = axes,
        col = par("fg"), lty = par("lty"), lwd = par("lwd"),
	add = FALSE, \dots)
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
  \item{labels}{a vector giving the labels for the contour lines.  If
    \code{NULL} then the levels are used as labels, otherwise this is
    coerced by \code{\link{as.character}}.}
  \item{labcex}{\code{cex} for contour labelling.  This is an absolute
    size, not a multiple of \code{par("cex")}.}
  \item{drawlabels}{logical.  Contours are labelled if \code{TRUE}.}
  \item{method}{character string specifying where the labels will be
    located. Possible values are \code{"simple"}, \code{"edge"} and
    \code{"flattest"} (the default). See the Details section.}
  \item{vfont}{if a character vector of length 2 is specified, then Hershey
    vector fonts are used for the contour labels.
    The first element of the vector
    selects a typeface and the second element selects a fontindex
    (see \code{\link{text}} for more information).}
  \item{xlim, ylim, zlim}{x-, y- and z-limits for the plot.}
  \item{axes, frame.plot}{logical indicating whether axes or a box
    should be drawn, see \code{\link{plot.default}}.}
  \item{col}{color for the lines drawn.}
  \item{lty}{line type for the lines drawn.}
  \item{lwd}{line width for the lines drawn.}
  \item{add}{logical. If \code{TRUE}, add to a current plot.}
  \item{\dots}{additional arguments to \code{\link{plot.window}},
    \code{\link{title}}, \code{\link{Axis}} and \code{\link{box}},
    typically graphical parameters such as \code{cex.axis}.}
}
\details{
  \code{contour} is a generic function with only a default method in
  base \R.

  The methods for positioning the labels on contours are \code{"simple"}
  (draw at the edge of the plot, overlaying the contour line),
  \code{"edge"} (draw at the edge of the plot, embedded in the contour
  line, with no labels overlapping) and \code{"flattest"}
  (draw on the flattest section of the contour, embedded in the
  contour line, with no labels overlapping).  The second and third may
  not draw a label on every contour line.

  For information about vector fonts, see the
  help for \code{\link{text}} and \code{\link{Hershey}}.

  Notice that \code{contour} interprets the \code{z} matrix as a table of
  \code{f(x[i], y[j])} values, so that the x axis corresponds to row
  number and the y axis to column number, with column 1 at the bottom,
  i.e. a 90 degree clockwise rotation of the conventional textual
  layout.

  Alternatively, use \code{\link[lattice]{contourplot}} from the
  \pkg{lattice} package where the \code{\link{formula}} notation allows
  to use vectors \code{x,y,z} of the same length.

  There is limited control over the axes and frame as arguments
  \code{col}, \code{lwd} and \code{lty} refer to the contour lines
  (rather than being general graphical parameters).  For more control,
  add contours to a plot, or add axes and frame to a contour plot.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{options}("max.countour.segments")} for the maximal
  complexity of a single contour line.

  \code{\link[grDevices]{contourLines}},
  \code{\link{filled.contour}} for \dQuote{color-filled} contours,
  \code{\link[lattice]{contourplot}} (and \code{\link[lattice]{levelplot}})
  from package \pkg{lattice}.  Further, \code{\link{image}} and the
  graphics demo which can be invoked as \code{demo(graphics)}.
}
\examples{
x <- -6:16
op <- par(mfrow = c(2, 2))
contour(outer(x, x), method = "edge", vfont = c("sans serif", "plain"))
z <- outer(x, sqrt(abs(x)), FUN = "/")
image(x, x, z)
contour(x, x, z, col = "pink", add = TRUE, method = "edge",
	vfont = c("sans serif", "plain"))
contour(x, x, z, ylim = c(1, 6), method = "simple", labcex = 1)
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2, method = "simple")
par(op)

## Persian Rug Art:
x <- y <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(x^2, y^2, "+"))
opar <- par(mfrow = c(2, 2), mar = rep(0, 4))
for(f in pi^(0:3))
  contour(cos(r^2)*exp(-r/f),
          drawlabels = FALSE, axes = FALSE, frame = TRUE)

rx <- range(x <- 10*1:nrow(volcano))
ry <- range(y <- 10*1:ncol(volcano))
ry <- ry + c(-1,1) * (diff(rx) - diff(ry))/2
tcol <- terrain.colors(12)
par(opar); opar <- par(pty = "s", bg = "lightcyan")
plot(x = 0, y = 0,type = "n", xlim = rx, ylim = ry, xlab = "", ylab = "")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = tcol[8], border = "red")
contour(x, y, volcano, col = tcol[2], lty = "solid", add = TRUE,
	vfont = c("sans serif", "plain"))
title("A Topographic Map of Maunga Whau", font = 4)
abline(h = 200*0:4, v = 200*0:4, col = "lightgray", lty = 2, lwd = 0.1)

## contourLines produces the same contour lines as contour
line.list <- contourLines(x, y, volcano)
plot(x = 0, y = 0,type = "n", xlim = rx, ylim = ry, xlab = "", ylab = "")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = tcol[8], border = "red")
contour(x, y, volcano, col = tcol[2], lty = "solid", add = TRUE,
             vfont = c("sans serif", "plain"))
templines <- function(clines) {
  lines(clines[[2]], clines[[3]])
}
invisible(lapply(line.list, templines))
par(opar)
}
\keyword{hplot}
\keyword{aplot}
