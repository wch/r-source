\name{isoreg}
\title{Isotonic / Monotone Regression}
\alias{isoreg}
%\alias{fitted.isoreg}
%\alias{residuals.isoreg}
\concept{monotonic regression}
\description{
  Compute the isotonic (monotonely increasing nonparametric) least
  squares regression which is piecewise constant.
}
\usage{
isoreg(x, y = NULL)
}
\arguments{
  \item{x, y}{in \code{isoreg}, coordinate vectors of the regression
    points.  Alternatively a single \dQuote{plotting} structure can be
    specified: see \code{\link{xy.coords}}.
  }
}
\details{
  The algorithm determines the convex minorant \eqn{m(x)} of the
  \emph{cumulative} data (i.e., \code{cumsum(y)}) which is piecewise
  linear and the result is \eqn{m'(x)}, a step function with level
  changes at locations where the convex \eqn{m(x)} touches the
  cumulative data polygon and changes slope.\cr
  \code{\link{as.stepfun}()} returns a \code{\link{stepfun}}
  object which can be more parsimonious.
}
\value{
  \code{isoreg()} returns an object of class \code{isoreg} which is
  basically a list with components
  \item{x}{original (constructed) abscissa values \code{x}.}
  \item{y}{corresponding y values.}
  \item{yf}{fitted values corresponding to \emph{ordered} x values.}
  \item{yc}{cumulative y values corresponding to \emph{ordered} x values.}
  \item{iKnots}{integer vector giving indices where the fitted curve jumps,
    i.e., where the convex minorant has kinks.}
  \item{isOrd}{logical indicating if original x values were ordered
    increasingly already.}
  \item{ord}{\code{if(!isOrd)}: integer permutation \code{\link{order}(x)} of
    \emph{original} \code{x}.}
  \item{call}{the \code{\link{call}} to \code{isoreg()} used.}
}
\note{
  The code should be improved to accept \emph{weights} additionally and
  solve the corresponding weighted least squares problem.\cr
  \dQuote{Patches are welcome!}
}
\references{
  Barlow, R. E., Bartholomew, D. J., Bremner, J. M., and Brunk, H. D. (1972)
  \emph{Statistical inference under order restrictions}; Wiley, London.

  Robertson, T., Wright,F. T. and Dykstra, R. L. (1988)
  \emph{Order Restricted Statistical Inference}; Wiley, New York.
}
%%\author{Original C code by Brian Ripley; all else: Martin Maechler}
\seealso{the plotting method \code{\link{plot.isoreg}} with more examples;
  \code{\link[MASS]{isoMDS}()} from the \pkg{MASS} package internally
  uses isotonic regression.
}
\examples{
(ir <- isoreg(c(1,0,4,3,3,5,4,2,0)))
plot(ir, plot.type = "row")

(ir3 <- isoreg(y3 <- c(1,0,4,3,3,5,4,2, 3)))# last "3", not "0"
(fi3 <- as.stepfun(ir3))
(ir4 <- isoreg(1:10, y4 <- c(5, 9, 1:2, 5:8, 3, 8)))
cat("R^2 =", formatC(sum(residuals(ir4)^2) / (9*var(y4)), dig=2),"\n")
}
\keyword{regression}
\keyword{smooth}
