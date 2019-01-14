% File src/library/stats/man/splinefun.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{splinefun}
\alias{spline}
\alias{splinefun}
\alias{splinefunH}
\title{Interpolating Splines}
\description{
  Perform cubic (or Hermite) spline interpolation of given data points,
  returning either a list of points obtained by the interpolation or a
  \emph{function} performing the interpolation.
}
\usage{
splinefun(x, y = NULL,
          method = c("fmm", "periodic", "natural", "monoH.FC", "hyman"),
          ties = mean)

spline(x, y = NULL, n = 3*length(x), method = "fmm",
       xmin = min(x), xmax = max(x), xout, ties = mean)

splinefunH(x, y, m)
}
\arguments{
  \item{x, y}{vectors giving the coordinates of the points to be
    interpolated.  Alternatively a single plotting structure can be
    specified: see \code{\link{xy.coords}}.

    \code{y} must be increasing or decreasing for \code{method = "hyman"}.
  }
  \item{m}{(for \code{splinefunH()}): vector of \emph{slopes}
    \eqn{m_i}{m[i]} at the points \eqn{(x_i,y_i)}{(x[i],y[i])}; these
    together determine the \bold{H}ermite \dQuote{spline} which is
    piecewise cubic, (only) \emph{once} differentiable continuously.}
  \item{method}{specifies the type of spline to be used.  Possible
    values are \code{"fmm"}, \code{"natural"}, \code{"periodic"},
    \code{"monoH.FC"} and \code{"hyman"}.  Can be abbreviated.}
  \item{n}{if \code{xout} is left unspecified, interpolation takes place
       at \code{n} equally spaced points spanning the interval
       [\code{xmin}, \code{xmax}].}
  \item{xmin, xmax}{left-hand and right-hand endpoint of the
       interpolation interval (when \code{xout} is unspecified).}
  \item{xout}{an optional set of values specifying where interpolation
    is to take place.}
  \item{ties}{handling of tied \code{x} values.  The string
    \code{"ordered"} or a function (or the name of a function) taking a
    single vector argument and returning a single number or a length-2
    \code{\link{list}} of both, see \code{\link{approx}} and its
    \sQuote{Details} section, and the example below.}
}
\details{
  The inputs can contain missing values which are deleted, so at least
  one complete \code{(x, y)} pair is required.
  If \code{method = "fmm"}, the spline used is that of Forsythe, Malcolm
  and Moler (an exact cubic is fitted through the four points at each
  end of the data, and this is used to determine the end conditions).
  Natural splines are used when \code{method = "natural"}, and periodic
  splines when \code{method = "periodic"}.

  The method \code{"monoH.FC"} computes a \emph{monotone} Hermite spline
  according to the method of Fritsch and Carlson.  It does so by
  determining slopes such that the Hermite spline, determined by
  \eqn{(x_i,y_i,m_i)}{(x[i],y[i],m[i])}, is monotone (increasing or
  decreasing) \bold{iff} the data are.

  Method \code{"hyman"} computes a \emph{monotone} cubic spline using
  Hyman filtering of an \code{method = "fmm"} fit for strictly monotonic
  inputs.

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

  \code{splinefun} returns a function with formal arguments \code{x} and
  \code{deriv}, the latter defaulting to zero.  This function
  can be used to evaluate the interpolating cubic spline
  (\code{deriv} = 0), or its derivatives (\code{deriv} = 1, 2, 3) at the
  points \code{x}, where the spline function interpolates the data
  points originally specified.  It uses data stored in its environment
  when it was created, the details of which are subject to change.
}
\section{Warning}{
  The value returned by \code{splinefun} contains references to the code
  in the current version of \R: it is not intended to be saved and
  loaded into a different \R session.  This is safer in \R >= 3.0.0.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988).
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Dougherty, R. L., Edelman, A. and Hyman, J. M. (1989)
  Positivity-, monotonicity-, or convexity-preserving cubic and quintic
  Hermite interpolation.
  \emph{Mathematics of Computation}, \bold{52}, 471--494.
  \doi{10.1090/S0025-5718-1989-0962209-1}.

  Forsythe, G. E., Malcolm, M. A. and Moler, C. B. (1977).
  \emph{Computer Methods for Mathematical Computations}.
  Wiley.

  Fritsch, F. N. and Carlson, R. E. (1980).
  Monotone piecewise cubic interpolation.
  \emph{SIAM Journal on Numerical Analysis}, \bold{17}, 238--246.
  \doi{10.1137/0717021}.

  Hyman, J. M. (1983).
  Accurate monotonicity preserving cubic interpolation.
  \emph{SIAM Journal on Scientific and Statistical Computing}, \bold{4},
  645--654.
  \doi{10.1137/0904045}.
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
\author{
  R Core Team.

  Simon Wood for the original code for Hyman filtering.
}

\examples{
require(graphics)

op <- par(mfrow = c(2,1), mgp = c(2,.8,0), mar = 0.1+c(3,3,3,1))
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
legend(6, 25, c("fmm","natural","periodic"), col = 2:4, lty = 1)

y <- sin((x-0.5)*pi)
f <- splinefun(x, y)
ls(envir = environment(f))
splinecoef <- get("z", envir = environment(f))
curve(f(x), 1, 10, col = "green", lwd = 1.5)
points(splinecoef, col = "purple", cex = 2)
curve(f(x, deriv = 1), 1, 10, col = 2, lwd = 1.5)
curve(f(x, deriv = 2), 1, 10, col = 2, lwd = 1.5, n = 401)
curve(f(x, deriv = 3), 1, 10, col = 2, lwd = 1.5, n = 401)
par(op)

## Manual spline evaluation --- demo the coefficients :
.x <- splinecoef$x
u <- seq(3, 6, by = 0.25)
(ii <- findInterval(u, .x))
dx <- u - .x[ii]
f.u <- with(splinecoef,
            y[ii] + dx*(b[ii] + dx*(c[ii] + dx* d[ii])))
stopifnot(all.equal(f(u), f.u))

## An example with ties (non-unique  x values):
set.seed(1); x <- round(rnorm(30), 1); y <- sin(pi * x) + rnorm(30)/10
plot(x, y, main = "spline(x,y)  when x has ties")
lines(spline(x, y, n = 201), col = 2)
## visualizes the non-unique ones:
tx <- table(x); mx <- as.numeric(names(tx[tx > 1]))
ry <- matrix(unlist(tapply(y, match(x, mx), range, simplify = FALSE)),
             ncol = 2, byrow = TRUE)
segments(mx, ry[, 1], mx, ry[, 2], col = "blue", lwd = 2)

## Another example with sorted x, but ties:
set.seed(8); x <- sort(round(rnorm(30), 1)); y <- round(sin(pi * x) + rnorm(30)/10, 3)
summary(diff(x) == 0) # -> 7 duplicated x-values
str(spline(x, y, n = 201, ties="ordered")) # all '$y' entries are NaN
## The default (ties=mean) is ok, but most efficient to use instead is
sxyo <- spline(x, y, n = 201, ties= list("ordered", mean))
sapply(sxyo, summary)# all fine now
plot(x, y, main = "spline(x,y, ties=list(\"ordered\", mean)  for when x has ties")
lines(sxyo, col="blue")

## An example of monotone interpolation
n <- 20
set.seed(11)
x. <- sort(runif(n)) ; y. <- cumsum(abs(rnorm(n)))
plot(x., y.)
curve(splinefun(x., y.)(x), add = TRUE, col = 2, n = 1001)
curve(splinefun(x., y., method = "monoH.FC")(x), add = TRUE, col = 3, n = 1001)
curve(splinefun(x., y., method = "hyman")   (x), add = TRUE, col = 4, n = 1001)
legend("topleft",
       paste0("splinefun( \"", c("fmm", "monoH.FC", "hyman"), "\" )"),
       col = 2:4, lty = 1, bty = "n")

## and one from Fritsch and Carlson (1980), Dougherty et al (1989)
x. <- c(7.09, 8.09, 8.19, 8.7, 9.2, 10, 12, 15, 20)
f <- c(0, 2.76429e-5, 4.37498e-2, 0.169183, 0.469428, 0.943740,
       0.998636, 0.999919, 0.999994)
s0 <- splinefun(x., f)
s1 <- splinefun(x., f, method = "monoH.FC")
s2 <- splinefun(x., f, method = "hyman")
plot(x., f, ylim = c(-0.2, 1.2))
curve(s0(x), add = TRUE, col = 2, n = 1001) -> m0
curve(s1(x), add = TRUE, col = 3, n = 1001)
curve(s2(x), add = TRUE, col = 4, n = 1001)
legend("right",
       paste0("splinefun( \"", c("fmm", "monoH.FC", "hyman"), "\" )"),
       col = 2:4, lty = 1, bty = "n")

## they seem identical, but are not quite:
xx <- m0$x
plot(xx, s1(xx) - s2(xx), type = "l",  col = 2, lwd = 2,
     main = "Difference   monoH.FC - hyman"); abline(h = 0, lty = 3)

x <- xx[xx < 10.2] ## full range: x <- xx .. does not show enough
ccol <- adjustcolor(2:4, 0.8)
matplot(x, cbind(s0(x, deriv = 2), s1(x, deriv = 2), s2(x, deriv = 2))^2,
        lwd = 2, col = ccol, type = "l", ylab = quote({{f*second}(x)}^2),
        main = expression({{f*second}(x)}^2 ~" for the three 'splines'"))
legend("topright",
       paste0("splinefun( \"", c("fmm", "monoH.FC", "hyman"), "\" )"),
       lwd = 2, col  =  ccol, lty = 1:3, bty = "n")
## --> "hyman" has slightly smaller  Integral f''(x)^2 dx  than "FC",
## here, and both are 'much worse' than the regular fmm spline.
}
\keyword{math}
\keyword{dplot}
