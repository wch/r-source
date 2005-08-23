\name{stepfun}
\alias{stepfun}
\alias{is.stepfun}
\alias{as.stepfun}
%\alias{as.stepfun.default}
\alias{print.stepfun}
\alias{summary.stepfun}
\alias{knots}
%\alias{knots.stepfun}
\title{Step Function Class}
\usage{
stepfun(x, y, f = as.numeric(right), ties = "ordered", right = FALSE)

is.stepfun(x)
knots(Fn, \dots)
as.stepfun(x, \dots)

\method{print}{stepfun}(x, digits = getOption("digits") - 2, \dots)

\method{summary}{stepfun}(object, \dots)
}
\arguments{
  \item{x}{numeric vector giving the knots or jump locations of the step
    function for \code{stepfun()}.  For the other functions, \code{x} is
    as \code{object} below.}
  \item{y}{numeric vector one longer than \code{x}, giving the heights of
    the function values \emph{between} the x values.}
  \item{f}{a number between 0 and 1, indicating how interpolation outside
    the given x values should happen.  See \code{\link{approxfun}}.}
  \item{ties}{Handling of tied \code{x} values. Either a function or
    the string \code{"ordered"}.  See  \code{\link{approxfun}}.}
  \item{right}{logical, indicating if the intervals should be closed on
    the right (and open on the left) or vice versa.}

  \item{Fn, object}{an \R object inheriting from \code{"stepfun"}.}
  \item{digits}{number of significant digits to use, see \code{\link{print}}.}
  \item{\dots}{potentially further arguments (required by the generic).}
}
\description{
  Given the vectors \eqn{(x_1,\ldots, x_n)}{(x[1],\ldots, x[n])} and
  \eqn{(y_0,y_1,\ldots, y_n)}{(y[0],y[1],\ldots, y[n])} (one value
  more!), \code{stepfun(x,y,\dots)} returns an interpolating
  \dQuote{step} function, say \code{fn}. I.e., \eqn{fn(t) =
    c}\eqn{_i}{[i]} (constant) for \eqn{t \in (x_i, x_{i+1})}{t in (
    x[i], x[i+1])} and at the abscissa values, if (by default)
  \code{right = FALSE}, \eqn{fn(x_i) = y_i}{fn(x[i]) = y[i]} and for
  \code{right = TRUE}, \eqn{fn(x_i) = y_{i-1}}{fn(x[i]) = y[i-1]}, for
  \eqn{i=1,\ldots,n}.

  The value of the constant \eqn{c_i}{c[i]} above depends on the
  \dQuote{continuity} parameter \code{f}.
  For the default, \code{right = FALSE, f = 0},
  \code{fn} is a \dQuote{cadlag} function, i.e., continuous at right,
  limit (\dQuote{the point}) at left.
  In general, \eqn{c_i}{c[i]} is interpolated in between the
  neighbouring \eqn{y} values,
  \eqn{c_i= (1-f) y_i + f\cdot y_{i+1}}{c[i] = (1-f)*y[i] + f*y[i+1]}.
  Therefore, for non-0 values of \code{f}, \code{fn} may no longer be a proper
  step function, since it can be discontinuous from both sides, unless
  \code{right = TRUE, f = 1} which is right-continuous.
}
\value{
  A function of class \code{"stepfun"}, say \code{fn}.
%
  There are methods available for summarizing (\code{"summary(.)"}),
  representing (\code{"print(.)"}) and plotting  (\code{"plot(.)"}, see
  \code{\link{plot.stepfun}}) \code{"stepfun"} objects.

  The \code{\link{environment}} of \code{fn} contains all the
  information needed;
    \item{"x","y"}{the original arguments}
    \item{"n"}{number of knots (x values)}
    \item{"f"}{continuity parameter}
    \item{"yleft", "yright"}{the function values \emph{outside} the knots}
    \item{"method"}{(always \code{== "constant"}, from
      \code{\link{approxfun}(.)}).}
  The knots are also available via \code{\link{knots}(fn)}.
}
\author{
  Martin Maechler, \email{maechler@stat.math.ethz.ch} with some basic
  code from Thomas Lumley.
}
\seealso{\code{\link{ecdf}} for empirical distribution functions as
  special step functions and \code{\link{plot.stepfun}} for \emph{plotting}
  step functions.

  \code{\link{approxfun}} and \code{\link{splinefun}}.
}
\examples{
y0 <- c(1,2,4,3)
sfun0  <- stepfun(1:3, y0, f = 0)
sfun.2 <- stepfun(1:3, y0, f = .2)
sfun1  <- stepfun(1:3, y0, f = 1)
sfun1c <- stepfun(1:3, y0, right=TRUE)# hence f=1
sfun0
summary(sfun0)
summary(sfun.2)

## look at the internal structure:
unclass(sfun0)
ls(envir = environment(sfun0))

x0 <- seq(0.5,3.5, by = 0.25)
rbind(x=x0, f.f0 = sfun0(x0), f.f02= sfun.2(x0),
      f.f1 = sfun1(x0), f.f1c = sfun1c(x0))
## Identities :
stopifnot(identical(y0[-1], sfun0 (1:3)),# right = FALSE
          identical(y0[-4], sfun1c(1:3)))# right = TRUE
}
\keyword{dplot}
