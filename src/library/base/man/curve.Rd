\name{curve}
\title{Draw Function Plots}
\usage{
curve(expr, from, to, n=101, \dots)
}
\alias{curve}
\arguments{
\item{expr}{an expression, written as a function of
\code{x} which will be plotted.}
\item{from,to}{the range over which the function will be plotted.}
\item{n}{the expression will be evaluated at \code{n}
points equally spaced over the range \code{[from, to]}.
The points determined in this way are then joined with
straight lines.}
\item{\dots}{graphical parameters can also be specified as arguments.}
}
\description{
This function draws a curve corresponding to the
given expression (in \code{x}) over the interval
\code{[from,to]}.
It is a quick hack which seems to serve a useful purpose,
but can give bad results for functions which are
discontinuous.
}
\value{
\code{NULL}.  For ``expensive'' \code{expr}essions, you should smarter tools.
}
\seealso{
  \code{\link{splinefun}} for spline interpolation, \code{\link{lines}}.
}
\examples{
curve(x^3-3*x, -2, 2)
curve(x^2-2, add = TRUE, col = "violet")
}
\keyword{hplot}
