\name{polygon}
\title{Polygon Drawing}
\usage{
polygon(x, y, col=NULL, border=NULL, \dots) {
}
\alias{polygon}
\arguments{
\item{x,y}{vectors containing the coordinates of the vertices
of the polygon.}
\item{col}{an integer specifying the color to be used in
filling the polygon.  The default is to leave polygons unfilled.}
\item{border}{the color to draw the border.  The default is to
leave the border undrawn.}
\item{\dots}{graphical parameters can be given as arguments to \code{polygon}.}
}
\description{
\code{polygon} draws the polygons whose vertices are
given in \code{x} and \code{y}.
Note that if neither of \code{col} or \code{border} is specified
then nothing is drawn.
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
polygon(xx, yy, col="gray")
title("Distance Between Brownian Motions")
}
\keyword{aplot}
