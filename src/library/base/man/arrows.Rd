\name{arrows}
\title{Add Arrows to a Plot}
\usage{
arrows(x0, y0, x1, y1, length = 0.25, angle = 30, code = 2,
       col = par("fg"), lty = NULL, xpd = FALSE)
segments(x0, y0, x1, y1, col = par("fg"), lty = par("lty"))
}
\alias{arrows}
\alias{segments}
\arguments{
  \item{x0,y0}{coordinates of points \bold{from} which to draw.}
  \item{x1,y1}{coordinates of points \bold{to} which to draw.}
  \item{length}{length of the edges of the arrow head (in inches).}
  \item{angle}{angle from the shaft of the arrow to the edge of the arrow head.}
  \item{code}{integer code, determining \emph{kind} of arrows to be drawn.}
  \item{col, lty, xpd}{usual graphical parameters as in \code{\link{par}}.}
}
\description{
\code{segments} draws a line segment and
\code{arrows} draws an arrow between the point \code{(x0[i], y0[i])}
and the point \code{(x1[i],y1[i])}.
If \code{code=2} an arrowhead is drawn at \code{(x0[i],y0[i])} and if
\code{code=1} an arrowhead is drawn at \code{(x1[i],y1[i])}.  If
\code{code=3} a head is drawn at both ends of the arrow.

The graphical parameters \code{col} and \code{lty} can be used
to specify a color an line texture for the line segments which
make up the segments or arrows (\code{col} may be a vector).
}
\seealso{
  \code{\link{polygon}} for slightly easier and less flexible line drawing and
  \code{\link{lines}} for the usual polygons.
}
\examples{
x <- runif(12); y <- rnorm(12)
i <- order(x,y); x <- x[i]; y <- y[i]
plot(x,y, main="arrows(.) and segments(.)")
## draw arrows from point to point :
s <- seq(length(x)-1)# one shorter than data
arrows(x[s], y[s], x[s+1], y[s+1], col= 1:3)
s <- s[-length(s)]
segments(x[s], y[s], x[s+2], y[s+2], col= 'pink')
}
\keyword{aplot}
