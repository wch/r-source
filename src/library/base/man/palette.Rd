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
This function is used to view or manipulate the color
palette which is used when a \code{col=} has a numeric index.

If \code{value} has length 1 it is taken to be the name of
a built in color palette.  If \code{value} has length greater
than 1 it is assumed to contain a description of the colors
which are to make up the new palette (either by name or
by RGB levels).

If \code{value} is omitted or has length 0, no change is made
the current palette.

Currently, the only built-in palette is \code{"default"}.
}
\value{
The palette which was in effect.  This is invisible unless
the argument is omitted.
}
\seealso{
  \code{\link{colors}} for the vector of built-in ``named'' colors;
  \code{\link{hsv}}, \code{\link{gray}}, \code{\link{rainbow}},
  \code{\link{terrain.colors}},\dots to construct colors.
}
\examples{
palette()           # obtain the current palette
palette(rainbow(6)) # six color rainbow
palette("default")  # reset back to the default
}
\keyword{color}
\keyword{sysdata}
