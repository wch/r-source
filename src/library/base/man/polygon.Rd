\name{polygon}
\title{Polygon Drawing}
\usage{
polygon(x, y = NULL, density = -1, angle = 45, border=par("fg"), \dots) {
}
\alias{polygon}
\arguments{
\item{x,y}{vectors containing the coordinates of the vertices
  of the polygon.}
\item{density}{density of fill pattern. \emph{NOT YET} implemented.}
\item{angle}{angle of fill pattern. \emph{NOT YET} implemented.}
\item{border}{the color to draw the border.  Defaults to
  \code{\link{par}("fg")}.  Use \code{border=0} to omit borders.} 
\item{\dots}{graphical parameters can be given as arguments to
  \code{polygon}, must importantly,}
\item{col}{an integer specifying the color to be used in
  filling the polygon.  The default is to leave polygons unfilled.}
}
\description{
\code{polygon} draws the polygons whose vertices are
given in \code{x} and \code{y}.
}
\seealso{
  \code{\link{segments}} for even more flexibility,
  \code{\link{lines}}, \code{\link{rect}}, \code{\link{box}}, \code{\link{abline}}.
}	
\examples{
n <- 100
xx <- c(0:n, n:0)
yy <- c(c(0,cumsum(rnorm(n))), rev(c(0,cumsum(rnorm(n)))))
plot   (xx, yy, type="n", xlab="Time", ylab="Distance")
polygon(xx, yy, col="gray", border = "red")
title("Distance Between Brownian Motions")
}
\keyword{aplot}
