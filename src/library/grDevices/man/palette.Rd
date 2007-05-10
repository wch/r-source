\name{palette}
\title{Set or View the Graphics Palette}
\usage{
palette(value)
}
\alias{palette}
\arguments{
  \item{value}{an optional character vector.}
}
\description{
  View or manipulate the color palette which is used when a \code{col=}
  has a numeric index.
}
\details{
  If \code{value} has length 1, it is taken to be the name of
  a built in color palette.  If \code{value} has length greater
  than 1 it is assumed to contain a description of the colors
  which are to make up the new palette (either by name or
  by RGB levels).

  If \code{value} is omitted or has length 0, no change is made
  the current palette.

  Currently, the only built-in palette is \code{"default"}.
}
\value{
  The palette which \emph{was} in effect.  This is \code{\link{invisible}}
  unless the argument is omitted.
}
\seealso{
  \code{\link{colors}} for the vector of built-in \dQuote{named} colors;
  \code{\link{hsv}}, \code{\link{gray}}, \code{\link{rainbow}},
  \code{\link{terrain.colors}},\dots to construct colors.

  \code{\link{colorRamp}} to interpolate colors, making custom palettes;
  \code{\link{col2rgb}} for translating colors to RGB 3-vectors.
}
\examples{
require(graphics)

palette()               # obtain the current palette
palette(rainbow(6))     # six color rainbow

(palette(gray(seq(0,.9,len=25)))) # gray scales; print old palette
matplot(outer(1:100,1:30), type='l', lty=1,lwd=2, col=1:30,
        main = "Gray Scales Palette",
        sub = "palette(gray(seq(0,.9,len=25)))")
palette("default")      # reset back to the default
}
\keyword{color}
\keyword{sysdata}
