\name{Palettes}
\title{Color Palettes}
\usage{
rainbow(n, s = 1, v = 1, start = 0, end = max(1,n - 1)/n, gamma = 1)
heat.colors(n)
terrain.colors(n)
topo.colors(n)
cm.colors(n)
}
\alias{rainbow}
\alias{heat.colors}
\alias{terrain.colors}
\alias{topo.colors}
\alias{cm.colors}
\arguments{
\item{n}{the number of colors (\eqn{\ge 1}{>= 1}) to be in the palette.}
\item{s,v}{the ``saturation'' and ``value'' to be used to
  complete the HSV color descriptions.}
\item{start}{the (corrected) hue in [0,1] at which the rainbow begins.}
\item{end}{the (corrected) hue in [0,1] at which the rainbow ends.}
\item{gamma}{the gamma correction, see \code{\link{hsv}(.., gamma)}.}
}
\description{
These functions create a vector of \code{n} ``contiguous'' colors.
Conceptually, all of these functions actually use (parts of) a line
cut out of the 3-dimensional color space, parametrized by
\code{\link{hsv}(h,s,v, gamma)}, where \code{gamma}\eqn{=1} for the
\emph{foo}\code{.colors} function, and hence,
equispaced hues in RGB space tend to cluster at
the red, green and blue primaries.

Some applications such as contouring require a palette
of colors which do not ``wrap around'' to give a final
color close to the starting one.
With \code{rainbow}, the parameters \code{start} and \code{end} can be used
to specify particular subranges of hues.
The following values can be used when generating
such a subrange:
red=0, yellow=\eqn{\frac 1 6}{1/6}, green=\eqn{\frac 2 6}{2/6}, cyan=\eqn{\frac 3 6}{3/6}, blue=\eqn{\frac 4 6}{4/6} and magenta=\eqn{\frac 5 6}{5/6}.
}
\value{A character vector, \code{cv}, of color names.  This can be used
  either to create a user--defined color palette for subsequent
  graphics by \code{\link{palette}(cv)}, a \code{col=} specification
  in graphics functions or in \code{par}.
}
\seealso{
\code{\link{colors}}, \code{\link{palette}},  \code{\link{hsv}},
\code{\link{rgb}}, \code{\link{gray}}.
}
\examples{
# A Color Wheel
piechart(rep(1,12), col=rainbow(12))

##------ Some palettes ------------
n <- if(.Device == "postscript") 64 else 16
## For screen, larger n may give color allocation problem
i <- 1:n
d <- n/20; dy <- 2*d; j <- n\%/\%4
plot(i,i+d, type='n', main=paste("color palettes;  n=",n))
rect(i-.5,     dy, i+.4,   j, col=rainbow(n, start=.7, end=.1))
text(2*j,    j+dy/4,		 "rainbow(n, start=.7, end=.1)")
rect(i-.5,   j+dy, i+.4, 2*j, col=heat.colors(n))
text(2*j,  2*j+dy/4,		 "heat.colors(n)")
rect(i-.5, 2*j+dy, i+.4, 3*j, col=terrain.colors(n))
text(2*j,  3*j+dy/4,		 "terrain.colors(n)")
rect(i-.5, 3*j+dy, i+.4, 4*j, col=topo.colors(n))
text(2*j,  4*j+dy/4,		 "topo.colors(n)")
rect(i-.5, 3*j+dy, i+.4, 4*j, col=cm.colors(n))
text(2*j,  4*j+dy/4,		 "cm.colors(n)")
}
\keyword{color}
\keyword{dplot}
