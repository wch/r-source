% File src/library/stats/man/approxfun.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{approxfun}
\alias{approx}
\alias{approxfun}
\title{Interpolation Functions}
\description{
  Return a list of points which linearly interpolate given data points,
  or a function performing the linear (or constant) interpolation.
}
\usage{
approx   (x, y = NULL, xout, method = "linear", n = 50,
          yleft, yright, rule = 1, f = 0, ties = mean, na.rm = TRUE)

approxfun(x, y = NULL,       method = "linear",
          yleft, yright, rule = 1, f = 0, ties = mean, na.rm = TRUE)
}
\arguments{
  \item{x, y}{numeric vectors giving the coordinates of the points to be
    interpolated.  Alternatively a single plotting structure can be
    specified: see \code{\link{xy.coords}}.}
  \item{xout}{an optional set of numeric values specifying where
    interpolation is to take place.}
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
  \item{rule}{an integer (of length 1 or 2) describing how interpolation
    is to take place outside the interval [\code{min(x)}, \code{max(x)}].
    If \code{rule} is \code{1} then \code{NA}s are returned for such
    points and if it is \code{2}, the value at the closest data extreme
    is used.  Use, e.g., \code{rule = 2:1}, if the left and right side
    extrapolation should differ.}
  \item{f}{for \code{method = "constant"} a number between 0 and 1
    inclusive, indicating a compromise between left- and
    right-continuous step functions. If \code{y0} and \code{y1} are
    the values to the left and right of the point then the value is
    \code{y0} if \code{f == 0}, \code{y1} if \code{f == 1}, and
    \code{ y0*(1-f)+y1*f} for intermediate values. In this way the result is
    right-continuous for \code{f == 0} and left-continuous for \code{f
    == 1}, even for non-finite \code{y} values.}
  \item{ties}{handling of tied \code{x} values.  The string
    \code{"ordered"} or a function (or the name of a function)
    taking a single vector argument and returning a single number
    or a \code{\link{list}} of both, e.g.,
    \code{list("ordered", mean)}, see \sQuote{Details}.}
  \item{na.rm}{logical specifying how missing values (\code{\link{NA}}'s)
    should be handled.  Setting \code{na.rm=FALSE} will propagate
    \code{NA}'s in \code{y} to the interpolated values, also depending on
    the \code{rule} set.  Note that in this case, \code{NA}'s in \code{x}
    are invalid, see also the examples.}
}
\details{
  The inputs can contain missing values which are deleted (if \code{na.rm}
  is true, i.e., by default), so at least
  two complete \code{(x, y)} pairs are required (for \code{method =
  "linear"}, one otherwise).  If there are duplicated (tied) \code{x}
  values and \code{ties} contains a function it is applied to the \code{y}
  values for each distinct \code{x} value to produce \code{(x,y)} pairs
  with unique \code{x}.
  Useful functions in this context include \code{\link{mean}},
  \code{\link{min}}, and \code{\link{max}}.

  If \code{ties = "ordered"} the \code{x} values are assumed to be already
  ordered (and unique) and ties are \emph{not} checked but kept if present.
  This is the fastest option for large \code{length(x)}.

  If \code{ties} is a \code{\link{list}} of length two, \code{ties[[2]]}
  must be a function to be applied to ties, see above, but if
  \code{ties[[1]]} is identical to \code{"ordered"}, the \code{x} values
  are assumed to be sorted and are only checked for ties.  Consequently,
  \code{ties = list("ordered", mean)} will be slightly more efficient than
  the default \code{ties = mean} in such a case.

  The first \code{y} value will be used for interpolation to the left and the last
  one for interpolation to the right.
}
\value{
  \code{approx} returns a list with components \code{x} and \code{y},
  containing \code{n} coordinates which interpolate the given data
  points according to the \code{method} (and \code{rule}) desired.

  The function \code{approxfun} returns a function performing (linear or
  constant) interpolation of the given data points.  For a given set of
  \code{x} values, this function will return the corresponding
  interpolated values.  It uses data stored in its environment when it
  was created, the details of which are subject to change.
}
\section{Warning}{
  The value returned by \code{approxfun} contains references to the code
  in the current version of \R: it is not intended to be saved and
  loaded into a different \R session.  This is safer for \R >= 3.0.0.
}
\seealso{
  \code{\link{spline}} and \code{\link{splinefun}} for spline
  interpolation.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
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
curve(f(x), 0, 11, col = "green2")
points(x, y)
is.function(fc <- approxfun(x, y, method = "const")) # TRUE
curve(fc(x), 0, 10, col = "darkblue", add = TRUE)
## different extrapolation on left and right side :
plot(approxfun(x, y, rule = 2:1), 0, 11,
     col = "tomato", add = TRUE, lty = 3, lwd = 2)

### Treatment of 'NA's -- are kept if  na.rm=FALSE :

xn <- 1:4
yn <- c(1,NA,3:4)
xout <- (1:9)/2
## Default behavior (na.rm = TRUE): NA's omitted; extrapolation gives NA
data.frame(approx(xn,yn, xout))
data.frame(approx(xn,yn, xout, rule = 2))# -> *constant* extrapolation
## New (2019-2020)  na.rm = FALSE: NA's are "kept"
data.frame(approx(xn,yn, xout, na.rm=FALSE, rule = 2))
data.frame(approx(xn,yn, xout, na.rm=FALSE, rule = 2, method="constant"))

## NA's in x[] are not allowed:
stopifnot(inherits( try( approx(yn,yn, na.rm=FALSE) ), "try-error"))

## Give a nice overview of all possibilities  rule * method * na.rm :
##             -----------------------------  ====   ======   =====
## extrapolations "N":= NA;   "C":= Constant :
rules <- list(N=1, C=2, NC=1:2, CN=2:1)
methods <- c("constant","linear")
ry <- sapply(rules, function(R) {
       sapply(methods, function(M)
        sapply(setNames(,c(TRUE,FALSE)), function(na.)
                 approx(xn, yn, xout=xout, method=M, rule=R, na.rm=na.)$y),
        simplify="array")
   }, simplify="array")
names(dimnames(ry)) <- c("x = ", "na.rm", "method", "rule")
dimnames(ry)[[1]] <- format(xout)
ftable(aperm(ry, 4:1)) # --> (4 * 2 * 2) x length(xout)  =  16 x 9 matrix
\dontshow{% functionality and consistency tests:
stopifnot(exprs = {
 identical(unname(ry),
   array(c(NA, 1, 1,   1, 1, 3, 3, 4, NA,      NA, 1,  1, NA, NA, 3, 3, 4, NA,
           NA, 1, 1.5, 2, 2.5, 3, 3.5, 4, NA,  NA, 1, NA, NA, NA, 3, 3.5, 4, NA,
            1, 1, 1,   1, 1, 3, 3, 4, 4,        1, 1,  1, NA, NA, 3, 3, 4, 4,
            1, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4,    1, 1, NA, NA, NA, 3, 3.5, 4, 4,
           NA, 1, 1,   1, 1, 3, 3, 4, 4,       NA, 1,  1, NA, NA, 3, 3, 4, 4,
           NA, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4,   NA, 1, NA, NA, NA, 3, 3.5, 4, 4,
            1, 1, 1,   1, 1, 3, 3, 4, NA,       1, 1,  1, NA, NA, 3, 3, 4, NA,
            1, 1, 1.5, 2, 2.5, 3, 3.5, 4, NA,   1, 1, NA, NA, NA, 3, 3.5, 4, NA),
         dim = c(9L, 2L, 2L, 4L)))
 identical(approxfun(xn,yn, method="constant", rule=2, na.rm=FALSE)(xout),
            as.vector(ry[,"FALSE", "constant","C"]))
 identical(approxfun(xn,yn, method="linear", rule=2:1, na.rm=FALSE)(xout),
            as.vector(ry[,"FALSE", "linear", "CN"]))
})
}

## Show treatment of 'ties' :

x <- c(2,2:4,4,4,5,5,7,7,7)
y <- c(1:6, 5:4, 3:1)
(amy <- approx(x, y, xout = x)$y) # warning, can be avoided by specifying 'ties=':
op <- options(warn=2) # warnings would be error
stopifnot(identical(amy, approx(x, y, xout = x, ties=mean)$y))
(ay <- approx(x, y, xout = x, ties = "ordered")$y)
stopifnot(amy == c(1.5,1.5, 3, 5,5,5, 4.5,4.5, 2,2,2),
          ay  == c(2, 2,    3, 6,6,6, 4, 4,    1,1,1))
approx(x, y, xout = x, ties = min)$y
approx(x, y, xout = x, ties = max)$y
options(op) # revert 'warn'ing level
}%% MM has nice utility plotting in MISC/approx-ex.R -- do in demo ?
\keyword{arith}
\keyword{dplot}
