\name{rgb}
\title{RGB Color Specification}
\usage{
rgb(red, green, blue, names=NULL)
}
\alias{rgb}
\description{
This function creates ``colors'' corresponding to
the given intensities of the red, green and blue primaries
(the intensities must lie between zero and one).
The names argument may be used to provide names for the
colors.

The values returned by \code{rgb} can be used with a
\code{col=} specification in graphics functions or in \code{par}.
}
\seealso{
\code{\link{rainbow}}, \code{\link{hsv}}, \code{\link{gray}}.
}
