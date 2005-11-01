\name{arrows}
\title{Add Arrows to a Plot}
\usage{
arrows(x0, y0, x1, y1, length = 0.25, angle = 30, code = 2,
       col = par("fg"), lty = par("lty"), lwd = par("lwd"),
       \dots)
}
\alias{arrows}
\arguments{
  \item{x0, y0}{coordinates of points \bold{from} which to draw.}
  \item{x1, y1}{coordinates of points \bold{to} which to draw.}
  \item{length}{length of the edges of the arrow head (in inches).}
  \item{angle}{angle from the shaft of the arrow to the edge of the
    arrow head.}
  \item{code}{integer code, determining \emph{kind} of arrows to be
    drawn.}
  \item{col, lty, lwd}{graphical parameters, possible vectors.}
  \item{\dots}{usual graphical parameters such as \code{xpd}: see
    \code{\link{par}}.}
}
\description{
  Draw arrows between pairs of points.
}
\details{
  For each \code{i}, an arrow is drawn between the point \code{(x0[i],
    y0[i])} and the point \code{(x1[i],y1[i])}.

  If \code{code=1} an arrowhead is drawn at \code{(x0[i],y0[i])} and if
  \code{code=2} an arrowhead is drawn at \code{(x1[i],y1[i])}.  If
  \code{code=3} a head is drawn at both ends of the arrow.  Unless
  \code{length = 0}, when no head is drawn.

  The graphical parameters \code{col}, \code{lty} and \code{lwd} can be
  vectors of length greater than one and will be recycled if ncessary.

  The direction of a zero-length arrow is indeterminate, and hence so is
  the direction of the arrowheads.  To allow for rounding error,
  arrowheads are omitted (with a warning) on any arrow of length less
  than 1/1000 inch.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{segments}} to draw segments.
}
\note{The first four arguments in the comparable S function are named
  \code{x1,y1,x2,y2}.
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
