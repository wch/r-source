\name{stepfun}
\title{Step Functions}
\usage{
fn <- stepfun(x, y, f=0)
is.stepfun(fn)
knots(fn)
plot(fn, \dots)
print(fn, ...)
summary(fn)
}
\alias{stepfun}
\alias{is.stepfun}
\alias{print.stepfun}
\alias{summary.stepfun}
\alias{knots}
\alias{knots.stepfun}
\arguments{
 \item{x}{numeric vector giving the ``knots'' or jump locations of the
   step function.}
 \item{y}{numeric vector one longer than \code{x}, giving the heights of
   the function values \emph{between} the x values.}
 \item{f}{a number between 0 and 1, indicating how interpolation outside
   the given x values should happen.  See  \code{\link{approxfun}}.}
 \item{fn}{an \R object inheriting from \code{"stepfun"}.}
}
\description{
  Given the vectors \eqn{(x_1,\ldots, x_n)}{(x[1],\ldots, x[n])} and
  \eqn{(y_0,y_1,\ldots, y_n)}{(y[0],y[1],\ldots, y[n])}  (one value more!),
  \code{stepfun(x,y,\dots)} returns an interpolating ``step'' function,
  say \code{fn}. I.e., \eqn{fn(t) = c}\eqn{_i}{[i]} (constant) for
  \eqn{t \in (x_i, x_{i+1})}{t in ( x[i], x[i+1])} and
  \eqn{fn(x_i) = y_i}{fn(x[i]) = y[i]} for \eqn{i=1,\ldots,n}.

  The value of the constant \eqn{c_i}{c[i]} above depends on the
  ``continuity'' parameter \code{f}.
  For the default, \code{f = 0}, \code{fn} is a ``cadlag'' function, i.e.
  continuous at right, limit (``the point'') at left.
  In general, \eqn{c_i}{c[i]} is interpolated in between the
  neighbouring \eqn{y} values,
  \eqn{c_i= (1-f) y_i + f\cdot y_{i+1}}{c[i] = (1-f)*y[i] + f*y[i+1]}.
  Therefore, for non-0 values of \code{f}, \code{fn} may no longer be a proper
  step function, since it can be discontinuous from both sides.
}
\value{
  A function of class \code{"stepfun"}, say \code{fn}.
  There are methods available for
  summarizing (\code{"summary(.)"}), representing
  (\code{"print(.)"}) and plotting  (\code{"plot(.)"}, see
  \code{\link{plot.stepfun}}) \code{"stepfun"} objects.

  The \code{\link{environment}} of \code{fn} contains all the
  information needed;
%%  \itemize{
%% have "\ \ " instead of "\item:
    \ \ \code{"x","y"}: the original arguments;
    \ \ \code{"n"}: number of knots (x values);
    \ \ \code{"f"}: continuity parameter;
    \ \ \code{"yleft", "yright"} the function values \emph{outside} the knots;
    \ \ \code{"method"} (always \code{== "constant"}; not used, from
        \code{\link{approxfun}(.)}).
%%  }
  The knots are also available by \code{\link{knots}(fn)}.
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
sfun0
summary(sfun0)
summary(sfun.2)

x0 <- seq(0.5,3.5, by = 0.25)
rbind(x=x0, f.f0 = sfun0(x0), f.f02= sfun.2(x0), f.f1 = sfun1(x0))
}
\keyword{iplot}

