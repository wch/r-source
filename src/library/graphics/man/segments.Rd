\name{segments}
\alias{segments}
\title{Add Line Segments to a Plot}
\description{
  Draw line segments between pairs of points.
}
\usage{
segments(x0, y0, x1, y1,
         col = par("fg"), lty = par("lty"), lwd = par("lwd"),
         \dots)
}
\arguments{
  \item{x0, y0}{coordinates of points \bold{from} which to draw.}
  \item{x1, y1}{coordinates of points \bold{to} which to draw.}
  \item{col, lty, lwd}{usual graphical parameters as in
    \code{\link{par}}, possibly vectors.  \code{NA} values in \code{col}
    cause the segment to be omitted.}
  \item{\dots}{further graphical parameters (from \code{\link{par}}),
    such as \code{xpd}.}
}
\details{
  For each \code{i}, a line segment is drawn between the point
  \code{(x0[i], y0[i])} and the point \code{(x1[i], y1[i])}.

  The graphical parameters \code{col}, \code{lty} and \code{lwd} can be
  vectors of length greater than one and will be recycled if ncessary.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{arrows}}, 
  \code{\link{polygon}} for slightly easier and less flexible line
  drawing, and
  \code{\link{lines}} for the usual polygons.
}
\examples{
x <- runif(12); y <- rnorm(12)
i <- order(x,y); x <- x[i]; y <- y[i]
plot(x, y, main="arrows(.) and segments(.)")
## draw arrows from point to point :
s <- seq(length(x)-1)# one shorter than data
arrows(x[s], y[s], x[s+1], y[s+1], col= 1:3)
s <- s[-length(s)]
segments(x[s], y[s], x[s+2], y[s+2], col= 'pink')
}
\keyword{aplot}
