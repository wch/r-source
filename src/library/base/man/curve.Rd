\name{curve}
\title{Draw Function Plots}
\alias{curve}
\alias{plot.function}
\usage{
curve(expr, from, to, n = 101, add = FALSE, type = "l",
      ylab = NULL, log = NULL, xlim = NULL, \dots)

\method{plot}{function}(x, from = 0, to = 1, xlim = NULL, \dots)
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
  \item{type, ylab, log, \dots}{graphical parameters can also be
    specified as arguments.
    \code{plot.function} passes all these to \code{curve}.}
}
\description{
  Draws a curve corresponding to the given function or expression (in
  \code{x}) over the interval \code{[from,to]}.
}
\details{
  The evaluation of \code{expr} is at \code{n} points equally spaced
  over the range \code{[from, to]}, possibly adapted to log scale.  The
  points determined in this way are then joined with straight lines.
  \code{x(t)} or \code{expr} (with \code{x} inside) must return a
  numeric of the same length as the argument \code{t} or \code{x}.

  If \code{add = TRUE}, \code{c(from,to)} default to \code{xlim} which
  defaults to the current x-limits.  Further, \code{log} is taken from the
  current plot when \code{add} is true.

  This used to be a quick hack which now seems to serve a useful purpose,
  but can give bad results for functions which are not smooth.

  For \dQuote{expensive} \code{expr}essions, you should use smarter tools.
}
\seealso{
  \code{\link{splinefun}} for spline interpolation, \code{\link{lines}}.
}
\examples{
op <- par(mfrow=c(2,2))
curve(x^3-3*x, -2, 2)
curve(x^2-2, add = TRUE, col = "violet")

plot(cos, xlim = c(-pi,3*pi), n = 1001, col = "blue")

chippy <- function(x) sin(cos(x)*exp(-x/2))
curve(chippy, -8, 7, n=2001)
curve(chippy, -8, -5)

for(ll in c("","x","y","xy"))
   curve(log(1+x), 1,100, log=ll, sub=paste("log= '",ll,"'",sep=""))
par(op)
}
\keyword{hplot}
