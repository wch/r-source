\name{curve}
\title{Draw Function Plots}
\usage{
curve(expr, from, to, n=101, \dots)
plot.function(fn, from=0, to=1, n=101, \dots)
}
\alias{curve}
\alias{plot.function}
\arguments{
\item{expr}{an expression written as a function of \code{x}, or
    alternatively a function which will be plotted.}
\item{fn}{a `vectorizing' numeric \R function.}
\item{from,to}{the range over which the function will be plotted.}
\item{n}{integer; the number of x values at which to evaluate.}
\item{\dots}{graphical parameters can also be specified as arguments.}
}
\description{
    Draws a curve corresponding to the given function or expression 
    (in \code{x}) over the interval \code{[from,to]}.
}
\details{
    The evaluation of \code{expr} is at \code{n} points equally spaced
    over the range \code{[from, to]}. 
    The points determined in this way are then joined with
    straight lines.
    \code{fn(x)} or \code{expr} (with \code{x} inside) must return a
    numeric of the same length as \code{x}.

    This used to be a quick hack which seems to serve a useful purpose,
    but can give bad results for functions which are not smooth.
}
\value{
\code{NULL}. For ``expensive'' \code{expr}essions, you should use smarter tools.
}
\seealso{
  \code{\link{splinefun}} for spline interpolation, \code{\link{lines}}.
}
\examples{
par(mfrow=c(2,2))
curve(x^3-3*x, -2, 2)
curve(x^2-2, add = TRUE, col = "violet")

curve(sin, -pi, 3*pi)
plot(cos)
chippy <- function(x) sin(cos(x)*exp(-x/2))
curve(chippy, -8, 7, n=2001)
}
\keyword{hplot}
