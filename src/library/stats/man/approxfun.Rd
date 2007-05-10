\name{approxfun}
\alias{approx}
\alias{approxfun}
\title{Interpolation Functions}
\description{
  Return a list of points which linearly interpolate given data points,
  or a function performing the linear (or constant) interpolation.
}
\usage{
approx   (x, y = NULL, xout, method="linear", n=50,
          yleft, yright, rule = 1, f = 0, ties = mean)

approxfun(x, y = NULL,       method="linear",
          yleft, yright, rule = 1, f = 0, ties = mean)
}
\arguments{
  \item{x, y}{vectors giving the coordinates of the points to be
    interpolated.  Alternatively a single plotting structure can be
    specified: see \code{\link{xy.coords}}.}
  \item{xout}{an optional set of values specifying where interpolation
    is to take place.}
  \item{method}{specifies the interpolation method to be used.  Choices
    are \code{"linear"} or \code{"constant"}.}
  \item{n}{If \code{xout} is not specified, interpolation takes place at
    \code{n} equally spaced points spanning the interval [\code{min(x)},
    \code{max(x)}].}
  \item{yleft}{the value to be returned when input \code{x} values are
    less than \code{min(x)}. The default is defined by the value
    of \code{rule} given below.}
  \item{yright}{the value to be returned when input \code{x} values are
    greater than \code{max(x)}. The default is defined by the value
    of \code{rule} given below.}
  \item{rule}{an integer describing how interpolation is to take place
    outside the interval [\code{min(x)}, \code{max(x)}].
    If \code{rule} is \code{1} then \code{NA}s are returned for such
    points and if it is \code{2}, the value at the closest data extreme
    is used.}
  \item{f}{For \code{method="constant"} a number between 0 and 1
    inclusive, indicating a compromise between left- and
    right-continuous step functions. If \code{y0} and \code{y1} are the
    values to the left and right of the point then the value is
    \code{y0*(1-f)+y1*f} so that \code{f=0} is right-continuous and
    \code{f=1} is left-continuous.}
  \item{ties}{Handling of tied \code{x} values.  Either a function
    with a single vector argument returning a single number result or
    the string \code{"ordered"}.}
}
\value{
  \code{approx} returns a list with components \code{x} and \code{y},
  containing \code{n} coordinates which interpolate the given data
  points according to the \code{method} (and \code{rule}) desired.

  The function \code{approxfun} returns a function performing (linear or
  constant) interpolation of the given data points.  For a given set of
  \code{x} values, this function will return the corresponding
  interpolated values.  This is often more useful than \code{approx}.
}
\details{
  The inputs can contain missing values which are deleted, so at least
  two complete \code{(x, y)} pairs are required (for \code{method =
  "linear"}, one otherwise).  If there are duplicated (tied) \code{x}
  values and \code{ties} is a function it is applied to the \code{y}
  values for each distinct \code{x} value.
  Useful functions in this context include \code{\link{mean}},
  \code{\link{min}}, and \code{\link{max}}.  If \code{ties="ordered"}
  the \code{x} values are assumed to be already ordered.  The first
  \code{y} value will be used for interpolation to the left and the last
  one for interpolation to the right.
}
\seealso{
  \code{\link{spline}} and \code{\link{splinefun}} for spline
  interpolation.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
require(graphics)

x <- 1:10
y <- rnorm(10)
par(mfrow = c(2,1))
plot(x, y, main = "approx(.) and approxfun(.)")
points(approx(x, y), col = 2, pch = "*")
points(approx(x, y, method = "constant"), col = 4, pch = "*")

f <- approxfun(x, y)
curve(f(x), 0, 10, col = "green")
points(x, y)
is.function(fc <- approxfun(x, y, method = "const")) # TRUE
curve(fc(x), 0, 10, col = "darkblue", add = TRUE)

## Show treatment of 'ties' :

x <- c(2,2:4,4,4,5,5,7,7,7)
y <- c(1:6, 5:4, 3:1)
approx(x,y, xout=x)$y # warning
(ay <- approx(x,y, xout=x, ties = "ordered")$y)
stopifnot(ay == c(2,2,3,6,6,6,4,4,1,1,1))
approx(x,y, xout=x, ties = min)$y
approx(x,y, xout=x, ties = max)$y

%%-- MM has nice utility plotting -- do in demo ?
}
\keyword{arith}
\keyword{dplot}
