\name{contour}
\title{Display contours}
\usage{
contour(x=, y=, z, nlevels = 10,
        levels = pretty(range(z, na.rm = TRUE), nlevels),
        labcex = 0, col = par("fg"), lty = par("lty"), add = FALSE)
}
\alias{contour}
\arguments{
\item{x,y}{locations of grid lines at which the values in \code{z}
are measured.  These must be in ascending order.
By default, equally spaced values from 0 to 1 are used.}
\item{z}{a matrix containing the values to be plotted (\code{NA}s are allowed).}
\item{nlevels}{number of contour levels desired \textbf{iff}
\code{levels} is not supplied.} 
\item{levels}{numeric vector of levels at which to draw contour lines.}
\item{labcex}{\code{cex} for contour labelling. (not yet used)}
\item{col}{color for the lines drawn}
\item{lty}{line type for the lines drawn}
\item{add}{logical. if \code{TRUE}, add to a current plot.}
}
\description{
Draws contour lines for the desired levels.
There is currently no documentation about the algorithm.
The source code is currently in \emph{\$RHOME/src/library/main/plot3d.c}.
}
\seealso{
\code{\link{image}} and the extensive example in

\code{system.file("demos/graphics","graphics")}.
}
\examples{
data("volcano")
rx <- range(x <- 10*1:nrow(volcano))
ry <- range(y <- 10*1:ncol(volcano))
ry <- ry + c(-1,1)*(diff(rx)-diff(ry))/2
tcol <- terrain.colors(12)
opar <- par(pty="s", bg="lightcyan")
plot(x=0,y=0,type='n', xlim=rx,  ylim=ry, xlab='', ylab='')
u <- par("usr")
rect(u[1],u[3], u[2],u[4], col = tcol[8], border='red')
contour(x, y, volcano, col=tcol[2], lty="solid", add=TRUE)
title("A Topographic Map of Maunga Whau",font=4)
abline(h=200*0:4,v=200*0:4, col= 'lightgray', lty=2, lwd=0.1)
}
