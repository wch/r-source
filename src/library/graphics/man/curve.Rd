% File src/library/graphics/man/curve.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{curve}
\title{Draw Function Plots}
\alias{curve}
\alias{plot.function}
\usage{
curve(expr, from = NULL, to = NULL, n = 101, add = FALSE, type = "l",
      ylab = NULL, log = NULL, xlim = NULL, \dots)

\method{plot}{function}(x, y = NULL, to = NULL, from = y, xlim = NULL, \dots)
}
\arguments{
  \item{expr}{an expression written as a function of \code{x}, or
    alternatively the name of a function which will be plotted.}
  \item{x}{a \sQuote{vectorizing} numeric \R function.}
  \item{from,to}{the range over which the function will be plotted.}
  \item{n}{integer; the number of x values at which to evaluate.}
  \item{add}{logical; if \code{TRUE} add to already existing plot.}
  \item{xlim}{numeric of length 2; if specified, it serves as default
    for \code{c(from, to)}.}
  \item{type}{plot type: see \code{\link{plot.default}}.}
  \item{y}{alias for \code{from} for compatibility with \code{plot()}}
  \item{ylab, log, \dots}{labels and graphical parameters can also be
    specified as arguments.
    \code{plot.function} passes all these to \code{curve}.}
}
\description{
  Draws a curve corresponding to the given function or, for
  \code{curve()} also an expression (in \code{x}) over the interval
  \code{[from,to]}.
}
\details{
  The evaluation of \code{expr} is at \code{n} points equally spaced
  over the range \code{[from, to]}, possibly adapted to log scale.  The
  points determined in this way are then joined with straight lines.
  \code{x(t)} or \code{expr} (with \code{x} inside) must return a
  numeric of the same length as the argument \code{t} or \code{x}.

  For \code{curve()}, if either of \code{from} or \code{to} is
  \code{NULL}, it defaults to the corresponding element of \code{xlim},
  and \code{xlim} defaults to the x-limits of the current plot.
  For \code{plot(<function>, ..)}, the defaults for \eqn{(from,to)} are
  \eqn{(0,1)}.

  \code{log} is taken from
  the current plot only when \code{add} is true, and otherwise
  defaults to \code{""} indicating linear scales on both axes.

  This used to be a quick hack which now seems to serve a useful purpose,
  but can give bad results for functions which are not smooth.

  For expensive-to-compute \code{expr}essions, you should use smarter tools.
}
\seealso{
  \code{\link{splinefun}} for spline interpolation, \code{\link{lines}}.
}
\examples{
plot(qnorm)
plot(qlogis, main = "The Inverse Logit : qlogis()")
abline(h=0, v=0:2/2, lty=3, col="gray")

curve(sin, -2*pi, 2*pi)
curve(tan, main = "curve(tan)  --> same x-scale as previous plot")

op <- par(mfrow=c(2,2))
curve(x^3-3*x, -2, 2)
curve(x^2-2, add = TRUE, col = "violet")

## simple and sophisticated, quite similar:
plot(cos, -pi,  3*pi)
plot(cos, xlim = c(-pi,3*pi), n = 1001, col = "blue", add=TRUE)

chippy <- function(x) sin(cos(x)*exp(-x/2))
curve(chippy, -8, 7, n=2001)
plot (chippy, -8, -5)

for(ll in c("","x","y","xy"))
   curve(log(1+x), 1,100, log=ll, sub=paste("log= '",ll,"'",sep=""))
par(op)
}
\keyword{hplot}
