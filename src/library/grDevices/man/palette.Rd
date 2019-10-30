% File src/library/grDevices/man/palette.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{palette}
\alias{palette}
\alias{palette.pals}
\alias{palette.colors}
\title{Set or View the Graphics Palette}
\usage{
palette(value)
palette.pals()
palette.colors(n = NULL, palette = "Okabe-Ito", alpha = 1)
}
\arguments{
  \item{value}{an optional character vector specifying a new palette
    (see Details).}
  \item{n}{the number of colors to select from a palette.}
  \item{palette}{a valid palette name (one of \code{palette.pals()}).}
  \item{alpha}{an alpha-transparency level.}
}
\description{
  View or manipulate the color palette which is used when a \code{col=}
  has a numeric index.
}
\details{
  The color palette and referring to colors by number (see
  e.g.\sspace{}\code{\link{par}}) was provided for compatibility with S: in \R
  it is almost always better to specify colours by name.

  If \code{value} has length 1, it is taken to be the name of a built-in
  color palette.  The available palette names are returned by
  \code{palette.pals()}.  It is also possible to specify \code{"default"}.

  If \code{value} has length greater than 1 it is assumed to contain a
  description of the colors which are to make up the new palette.
  The maximum size for a palette is 1024
  entries.

  If \code{value} is omitted, no change is made to the current palette.

  There is only one palette setting for all devices in an \R session.  If
  the palette is changed, the new palette applies to all subsequent
  plotting.

  The current palette also applies to re-plotting (for example if an
  on-screen device is resized or \code{\link{dev.copy}} or
  \code{\link{replayPlot}} is used).  The palette is recorded on the
  displaylist at the start of each page and when it is changed.
}

\value{
  A character vector giving the colors from the
  palette which \emph{was} in effect.
  This is \code{\link{invisible}} unless the argument is omitted.
}
\seealso{
  \code{\link{colors}} for the vector of built-in named colors;
  \code{\link{hsv}}, \code{\link{gray}}, \code{\link{rainbow}},
  \code{\link{terrain.colors}}, \dots to construct colors.

  \code{\link{adjustcolor}}, e.g., for tweaking existing palettes;
  \code{\link{colorRamp}} to interpolate colors, making custom palettes;
  \code{\link{col2rgb}} for translating colors to RGB 3-vectors.
}
\examples{
require(graphics)

palette()               # obtain the current palette
palette(rainbow(6))     # six color rainbow

(palette(gray(seq(0,.9,len = 25)))) # gray scales; print old palette
matplot(outer(1:100, 1:30), type = "l", lty = 1,lwd = 2, col = 1:30,
        main = "Gray Scales Palette",
        sub = "palette(gray(seq(0, .9, len=25)))")
palette("default")      # reset back to the default

## on a device where alpha-transparency is supported,
##  use 'alpha = 0.3' transparency with the default palette :
mycols <- adjustcolor(palette(), alpha.f = 0.3)
opal <- palette(mycols)
x <- rnorm(1000); xy <- cbind(x, 3*x + rnorm(1000))
plot (xy, lwd = 2,
       main = "Alpha-Transparency Palette\n alpha = 0.3")
xy[,1] <- -xy[,1]
points(xy, col = 8, pch = 16, cex = 1.5)
palette("default")

## List available built-in palettes
palette.pals()

## Demonstrate the colors 1:8 in different palettes using a custom matplot()
sinplot <- function() {
    x <- outer(
	seq(-pi, pi, length.out = 50),
	seq(0, pi, length.out = 8),
	function(x, y) sin(x - y)
    )
    matplot(x, type = "l", lwd = 4, lty = 1, col = 1:8, ylab = "")
}
sinplot()
palette("R3")
sinplot()
palette("Okabe-Ito")
sinplot()
palette("Tableau")
sinplot()

palette("default")
}
\keyword{color}
\keyword{sysdata}
