\name{polygon}
\alias{polygon}
\title{Polygon Drawing}
\description{
  \code{polygon} draws the polygons whose vertices are
  given in \code{x} and \code{y}.
}
\usage{
polygon(x, y = NULL, col = NA, border = NULL,
        lty = NULL, xpd = NULL, density = -1, angle = 45, \dots)
}
\arguments{
  \item{x,y}{vectors containing the coordinates of the vertices
    of the polygon.}
  \item{col}{the color for filling the polygon.
    The default, \code{NA}, is to leave polygons unfilled.}
  \item{border}{the color to draw the border.  The default, \code{NULL},
    uses \code{\link{par}("fg")}.  Use \code{border=0} to omit borders.}
  \item{lty}{the line type to be used, as in \code{\link{par}}.}
  \item{xpd}{(where) should clipping take place?  Defaults to
    \code{\link{par}("xpd")}.}
  \item{density}{density of fill pattern. \emph{NOT YET} implemented.}
  \item{angle}{angle of fill pattern. \emph{NOT YET} implemented.}
  \item{\dots}{graphical parameters can be given as arguments to
    \code{polygon}.}
}
\details{
  The coordinates can be passed in a plotting structure
  (a list with \code{x} and \code{y} components), a two-column matrix,
  \dots.  See \code{\link{xy.coords}}.

  It is assumed that the polygon is closed by joining the last point to
  the first point.

  The coordinates can contain missing values: see the description under
  Details section of the help for \code{\link{lines}}. 
}
\seealso{
  \code{\link{segments}} for even more flexibility, \code{\link{lines}},
  \code{\link{rect}}, \code{\link{box}}, \code{\link{abline}}.
}
\examples{
x <- c(1:9,8:1)
y <- c(1,2*(5:3),2,-1,17,9,8,2:9)
op <- par(mfcol=c(3,1))
for(xpd in c(FALSE,TRUE,NA)) {
  plot(1:10, main=paste("xpd =", xpd)) ; box("figure", col = "pink", lwd=3)
  polygon(x,y, xpd=xpd, col = "orange", lty=2, lwd=2, border = "red")
}
par(op)

n <- 100
xx <- c(0:n, n:0)
yy <- c(c(0,cumsum(rnorm(n))), rev(c(0,cumsum(rnorm(n)))))
plot   (xx, yy, type="n", xlab="Time", ylab="Distance")
polygon(xx, yy, col="gray", border = "red")
title("Distance Between Brownian Motions")
}
\keyword{aplot}
