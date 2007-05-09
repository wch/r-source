\name{abline}
\alias{abline}
\title{Add Straight Lines to a Plot}
\description{
  This function adds one or more straight lines through the current plot.
}
\usage{
abline(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL,
       coef = NULL, untf = FALSE, ...)
}
\arguments{
  \item{a, b}{the intercept and slope, single values.}
  \item{untf}{logical asking whether to \emph{untransform}.  See Details.}
  \item{h}{the y-value(s) for horizontal line(s).}
  \item{v}{the x-value(s) for vertical line(s).}
  \item{coef}{a vector of length two giving the intercept and slope.}
  \item{reg}{an object with a \code{\link{coef}} method.  See Details.}
  \item{\dots}{graphical parameters such as
    \code{col}, \code{lty} and \code{lwd} (possibly as vectors: see
    Details) and the line characteristics \code{lend}, \code{ljoin} and
    \code{lmitre}.}
}
\details{
  Typical usages are
\preformatted{
abline(a, b, untf = FALSE, \dots)
abline(h=, untf = FALSE, \dots)
abline(v=, untf = FALSE, \dots)
abline(coef=, untf = FALSE, \dots)
abline(reg=, untf = FALSE, \dots)
}
  The first form specifies the line in intercept/slope form
  (alternatively \code{a} can be specified on its own and is taken to
  contain the slope and intercept in vector form).

  The \code{h=} and \code{v=} forms draw horizontal and vertical lines
  at the specified coordinates.

  The \code{coef} form specifies the line by a vector containing the
  slope and intercept.

  \code{reg} is a regression object with a \code{\link{coef}} method.
  If this returns a vector of length 1 then the value is taken to be the
  slope of a line through the origin, otherwise, the first 2 values are
  taken to be the intercept and slope.

  If \code{untf} is true, and one or both axes are log-transformed, then
  a curve is drawn corresponding to a line in original coordinates,
  otherwise a line is drawn in the transformed coordinate system. The
  \code{h} and \code{v} parameters always refer to original coordinates.

  The graphical parameters \code{col}, \code{lty} and \code{lwd}
  can be specified; see \code{\link{par}} for details.  For the
  \code{h=} and \code{v=} usages they can be vectors of length greater
  than one, recycled as necessary.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}
\seealso{
  \code{\link{lines}} and \code{\link{segments}} for connected and
  arbitrary lines given by their \emph{endpoints}.
  \code{\link{par}}.
}
\examples{
## Setup up coordinate system (with x==y aspect ratio):
plot(c(-2,3), c(-1,5), type = "n", xlab="x", ylab="y", asp = 1)
## the x- and y-axis, and an integer grid
abline(h=0, v=0, col = "gray60")
text(1,0, "abline( h = 0 )", col = "gray60", adj = c(0, -.1))
abline(h = -1:5, v = -2:3, col = "lightgray", lty=3)
abline(a=1, b=2, col = 2)
text(1,3, "abline( 1, 2 )", col=2, adj=c(-.1,-.1))

## Simple Regression Lines:
require(stats)
sale5 <- c(6, 4, 9, 7, 6, 12, 8, 10, 9, 13)
plot(sale5)
abline(lsfit(1:10,sale5))
abline(lsfit(1:10,sale5, intercept = FALSE), col= 4) # less fitting

z <- lm(dist ~ speed, data = cars)
plot(cars)
abline(z) # equivalent to abline(reg = z) or
abline(coef = coef(z))

## trivial intercept model
abline(mC <- lm(dist ~ 1, data = cars)) ## the same as
abline(a = coef(mC), b = 0, col = "blue")
}
\keyword{aplot}
