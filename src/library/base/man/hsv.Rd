\name{hsv}
\title{HSV Color Specification}
\usage{
hsv(h=1, s=1, v=1, gamma=1)
}
\alias{hsv}
\arguments{
\item{h,s,v}{numeric vectors of values in the range
  \code{[0,1]} for ``hue'', ``saturation'' and ``value''
  to be combined to form a vector of colors.  Values in shorter
  arguments are recycled.}
\item{gamma}{a ``gamma correction'', see ...}
}
\value{
This function creates a vector of ``colors'' corresponding to
the given values in HSV space.  The values returned by \code{hsv}
can be used with a \code{col=} specification in graphics functions or in
\code{par}.
}
\seealso{
\code{\link{rainbow}}, \code{\link{rgb}}, \code{\link{gray}}.
}
\examples{
hsv(.5,.5,.5)
}
\keyword{color}
\keyword{dplot}
