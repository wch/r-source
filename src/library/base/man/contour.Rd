\name{contour}
\title{Display Contours}
\usage{
contour(x = seq(0, 1, len = nrow(z)), y = seq(0, 1, len = ncol(z)),
        z,
        nlevels = 10, levels = pretty(zlim, nlevels),
        xlim = range(x, finite = TRUE),
        ylim = range(y, finite = TRUE),
        zlim = range(z, finite = TRUE),
        labcex = 0,
        col = par("fg"), lty = par("lty"), add = FALSE, \dots)
}
\alias{contour}
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
  \item{labcex}{\code{cex} for contour labelling. (not yet used)}
  \item{xlim, ylim, zlim}{x-, y- and z-limits for the plot}
  \item{col}{color for the lines drawn}
  \item{lty}{line type for the lines drawn}
  \item{add}{logical. If \code{TRUE}, add to a current plot.}
  \item{\dots}{additional graphical parameters (see \code{\link{par}})
    and the arguments to \code{\link{title}} may also be supplied.}
}
\description{
  Draws contour lines for the desired levels.
  There is currently no documentation about the algorithm.
  The source code is in \file{\$R\_HOME/src/main/plot.c}.
}
\seealso{
  \code{\link{filled.contour}} for ``color-filled'' contours,
  \code{\link{image}} and the graphics demo which can be invoked as
  \code{demo(graphics)}.
}
\examples{
x <- -6:16
op <- par(mfrow = c(2, 2))
contour(outer(x, x))
z <- outer(x, sqrt(abs(x)), FUN = "/")
## Should not be necessary:
z[!is.finite(z)] <- NA
image(x, x, z)
contour(x, x, z, col = "pink", add = TRUE)
contour(x, x, z, ylim = c(1, 6))
contour(x, x, z, ylim = c(-6, 6), nlev = 20, lty = 2)
par(op)

## Persian Rug Art:
x <- y <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(x^2, y^2, "+"))
opar <- par(mfrow = c(2, 2), mar = rep(0, 4))
for(f in pi^(0:3)) contour(cos(r^2)*exp(-r/f))

data("volcano")
rx <- range(x <- 10*1:nrow(volcano))
ry <- range(y <- 10*1:ncol(volcano))
ry <- ry + c(-1,1) * (diff(rx) - diff(ry))/2
tcol <- terrain.colors(12)
par(opar); par(mfrow=c(1,1)); opar <- par(pty = "s", bg = "lightcyan")
plot(x = 0, y = 0,type = "n", xlim = rx, ylim = ry, xlab = "", ylab = "")
u <- par("usr")
rect(u[1], u[3], u[2], u[4], col = tcol[8], border = "red")
contour(x, y, volcano, col = tcol[2], lty = "solid", add = TRUE)
title("A Topographic Map of Maunga Whau", font = 4)
abline(h = 200*0:4, v = 200*0:4, col = "lightgray", lty = 2, lwd = 0.1)
par(opar)
}
\keyword{hplot}
\keyword{aplot}
