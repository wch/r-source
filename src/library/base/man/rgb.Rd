\name{rgb}
\title{RGB Color Specification}
\usage{
rgb(red, green, blue, names=NULL)
}
\alias{rgb}
\description{
This function creates ``colors'' corresponding to
the given intensities (between 0 and 1) of the red, green and blue primaries.
The names argument may be used to provide names for the
colors.

The values returned by \code{rgb} can be used with a
\code{col=} specification in graphics functions or in \code{par}.
}
\seealso{
  \code{\link{col2rgb}} the ``inverse'' for translating \R colors to RGB
  vectors.\cr
  \code{\link{rainbow}}, \code{\link{hsv}}, \code{\link{gray}}.
}
\examples{
rgb(0,1,0)
u01 <- seq(0,1, length=11); all(rgb(u01,u01,u01) == gray(u01))
reds <- rgb((0:15)/15, g=0,b=0, names=paste("red",0:15,sep="."))
reds
}
\keyword{color}
