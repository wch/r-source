\name{plot.window}
\title{Set up World Coordinates for Graphics Window}
\usage{
plot.window(xlim, ylim, log = "", asp = NA)
}
\alias{plot.window}
\arguments{
\item{xlim, ylim}{numeric of length 2, giving the x and y coordinates ranges.}
\item{log}{character; indicating which axes should be in log scale.}
\item{asp}{numeric, giving the \bold{asp}ect ratio y/x.}
}
\description{
  This function sets up the world coordinates for a graphics
  window.  Note that if \code{asp} is a finite positive value then
  the window is set up so that one data unit in the y direction
  is equal in length to \code{asp} \eqn{\times}{*} one data unit in the
  x direction.

  The special case \code{asp == 1} produces plots where distances
  between points are represented accurately on screen.  Values
  with \code{asp < 1} can be used to produce more accurate maps when
  using latitude and longitude.

Usually, one should rather use the higher level functions instead
and refer to their help pages for explanation of the arguments.
}
\seealso{
\code{\link{xy.coords}}, \code{\link{plot.xy}}, \code{\link{plot.default}}.
}
\keyword{aplot}
