\name{splinefun}
\alias{spline}
\alias{splinefun}
\title{Interpolating Splines}
\description{
  Perform cubic spline interpolation of given data points, returning
  either a list of points obtained by the interpolation or a function
  performing the interpolation.
}
\usage{
splinefun(x, y = NULL, method = "fmm")

spline(x, y = NULL, n = 3*length(x), method = "fmm",
       xmin = min(x), xmax = max(x))
}
\arguments{
  \item{x,y}{vectors giving the coordinates of the points to be
    interpolated.  Alternatively a single plotting structure can be
    specified: see \code{\link{xy.coords}.}}
  \item{method}{specifies the type of spline to be used.  Possible
    values are \code{"fmm"}, \code{"natural"} and \code{"periodic"}.}
  \item{n}{interpolation takes place at \code{n} equally spaced points
    spanning the interval [\code{xmin}, \code{xmax}].}
  \item{xmin}{left-hand endpoint of the interpolation interval.}
  \item{xmax}{right-hand endpoint of the interpolation interval.}
}
\details{
  The inputs can contain missing values which are deleted, so at least
  one complete \code{(x, y)} pair is required.
  If \code{method = "fmm"}, the spline used is that of Forsythe, Malcolm
  and Moler (an exact cubic is fitted through the four points at each
  end of the data, and this is used to determine the end conditions).
  Natural splines are used when \code{method = "natural"}, and periodic
  splines when \code{method = "periodic"}.

  These interpolation splines can also be used for extrapolation, that is
  prediction at points outside the range of \code{x}.  Extrapolation
  makes little sense for \code{method = "fmm"}; for natural splines it
  is linear using the slope of the interpolating curve at the nearest
  data point.
}
\value{
  \code{spline} returns a list containing components \code{x} and
  \code{y} which give the ordinates where interpolation took place and
  the interpolated values.

  \code{splinefun} returns a function which will perform cubic spline
  interpolation of the given data points.  This is often more useful
  than \code{spline}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Forsythe, G. E., Malcolm, M. A. and Moler, C. B. (1977)
  \emph{Computer Methods for Mathematical Computations}.
}
\seealso{
  \code{\link{approx}} and \code{\link{approxfun}} for constant and
  linear interpolation.

  Package \pkg{splines}, especially \code{\link[splines]{interpSpline}}
  and \code{\link[splines]{periodicSpline}} for interpolation splines.
  That package also generates spline bases that can be used for
  regression splines.

  \code{\link{smooth.spline}} for smoothing splines.
}
\examples{
op <- par(mfrow = c(2,1), mgp = c(2,.8,0), mar = .1+c(3,3,3,1))
n <- 9
x <- 1:n
y <- rnorm(n)
plot(x, y, main = paste("spline[fun](.) through", n, "points"))
lines(spline(x, y))
lines(spline(x, y, n = 201), col = 2)

y <- (x-6)^2
plot(x, y, main = "spline(.) -- 3 methods")
lines(spline(x, y, n = 201), col = 2)
lines(spline(x, y, n = 201, method = "natural"), col = 3)
lines(spline(x, y, n = 201, method = "periodic"), col = 4)
legend(6,25, c("fmm","natural","periodic"), col=2:4, lty=1)

f <- splinefun(x, y)
ls(envir = environment(f))
splinecoef <- eval(expression(z), envir = environment(f))
curve(f(x), 1, 10, col = "green", lwd = 1.5)
points(splinecoef, col = "purple", cex = 2)
par(op)
}
\keyword{math}
\keyword{dplot}
