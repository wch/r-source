\name{gray}
\title{Gray Level Specification}
\usage{
gray(level)
}
\alias{gray}
\value{
This function creates ``colors'' corresponding to the given level of gray.
The level must lie between zero and one, with zero indicating \code{"black"}
and one indicating \code{"white"}.

The values returned by \code{gray} can be used with a
\code{col=} specification in graphics functions or in \code{\link{par}}.
}
\seealso{
\code{\link{rainbow}}, \code{\link{hsv}}, \code{\link{rgb}}.
}
\examples{
gray(0:8 / 8)
}
\keyword{color}
