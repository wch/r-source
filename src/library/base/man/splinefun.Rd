\name{spline[fun]}
\title{Interpolating Splines}
\usage{
splinefun(x, y, method = "fmm")
spline(x, y, n = 3*length(x), method = "fmm",
       xmin = min(x), xmax = max(x))
}
\alias{splinefun}
\alias{spline}
\arguments{
  \item{x,y}{vectors giving the coordinates of the points to be
    interpolated.  Alternatively a single plotting structure can be
    specified.}
  \item{method}{specifies the type of spline to be used.  Possible
    values are \code{"fmm"}, \code{"natural"} and \code{"periodic"}.}
  \item{n}{interpolation takes place at \code{n} equally spaced points
    spanning the interval [\code{xmin}, \code{xmax}].} 
  \item{xmin}{left-hand endpoint of the interpolation interval.}
  \item{xmax}{right-hand endpoint of the interpolation interval.}
}
\description{
  \code{spline} performs cubic spline interpolation of the given data
  points.  It returns a list containing components \code{x} and \code{y}
  which give the ordinates where interpolation took place and the
  interpolated values.

  \code{splinefun} returns a function which will perform cubic spline
  interpolation of the given data points.  This is often more useful
  than \code{spline}.

  If \code{method="fmm"}, the spline used is that of Forsythe, Malcolm
  and Moler (an exact cubic is fitted through the four points at each
  end of the data, and this is used to determine the end conditions).
  Natural splines are used when \code{method="natural"}, and periodic
  splines when \code{method="periodic"}.
}
\references{
  Forsythe, G. E., M. A. Malcolm and C. B. Moler (1977).
  \emph{Computer Methods for Mathematical Computations}.
}
\seealso{
  \code{\link{approx}} and \code{\link{approxfun}} for constant and
  linear interpolation.
}
\examples{
x <- 1:10
y <- rnorm(10)
op <- par(mfrow = c(2,1), mgp = c(2,.8,0), mar = .1+c(3,3,3,1))
plot(x, y, main = "spline[fun](.) through 10 points")
lines(spline(x, y))
lines(spline(x, y, n = 201), col = 2)

f <- splinefun(x, y)
ls(envir = environment(f))
splinecoef <- eval(expression(z), envir = environment(f))
curve(f(x), 1, 10, col = "green", lwd = 1.5)
points(splinecoef, col = "purple", cex = 2)
par(op)
}
\keyword{math}
\keyword{dplot}
