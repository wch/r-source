\name{Palettes}
\title{Color Palettes}
\usage{
rainbow(n, s=1, v=1, start=0, end=(n-1)/n)
heat.colors(n)
terrain.colors(n)
topo.colors(n)
}
\alias{rainbow}
\alias{heat.colors}
\alias{terrain.colors}
\alias{topo.colors}
\arguments{
\item{n}{the number of colors to be in the palette.}
\item{s,v}{the ``saturation'' and ``value'' to be used to
complete the HSV color descriptions.}
\item{start}{the (corrected) hue at which the rainbow begins.}
\item{end}{the (corrected) hue at which the rainbow ends.}
}
\description{
This function creates a ``rainbow'' of \code{n} colors.
Equispaced hues in RGB space tend to cluster at
the red, green and blue primaries.

Some applications such as contouring require a pallette
of colors which do not ``wrap around'' to give a final
color close to the starting one.
The parameters \code{start} and \code{end} can be used
to specify particular subranges of hues.
The following values can be used when generating
such a subrange:
red=0, yellow=1/6, green=2/6, cyan=3/6, blue=4/6 and magenta=5/6.

The values returned by \code{rainbow} can be used with a
\code{col=} specification in graphics functions or in \code{par}.
}
\seealso{
\code{\link{colors}}, \code{\link{rgb}}, \code{\link{hsv}}, \code{\link{gray}},
\code{\link{heat.colors}}.
}
\examples{
# A Color Wheel
piechart(rep(1,12), col=rainbow(12))
}
