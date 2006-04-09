\name{hsv}
\alias{hsv}
\title{HSV Color Specification}
\description{
  Create a vector of colors from vectors specifying hue, saturation and
  value.
}
\usage{
hsv(h = 1, s = 1, v = 1, gamma = 1, alpha)
}
\arguments{
  \item{h,s,v}{numeric vectors of values in the range \code{[0,1]} for
    \dQuote{hue}, \dQuote{saturation} and \dQuote{value} to be combined
    to form a vector of colors.  Values in shorter arguments are
    recycled.}
  \item{gamma}{a \dQuote{gamma correction} exponent, \eqn{\gamma}}
  \item{alpha}{numeric value in the range \code{[0,1]} for
    alpha transparency channel (0 means transparent and 1 means opaque).}
}
\value{
  This function creates a vector of \dQuote{colors} corresponding to the
  given values in HSV space.  The values returned by \code{hsv} can be
  used with a \code{col=} specification in graphics functions or in
  \code{par}.
}
\section{Gamma correction}{
  For each color, \eqn{(r,g,b)} in RGB space (with all values in \eqn{[0,1]}),
  the final color corresponds to \eqn{(r^{\gamma}, g^{\gamma}, b^{\gamma})}.
}
\seealso{
  \code{\link{hcl}},
  \code{\link{rainbow}},
  \code{\link{rgb}},
  \code{\link{gray}}.
}
\examples{
hsv(.5,.5,.5)

## Look at gamma effect:
n <- 20;  y <- -sin(3*pi*((1:n)-1/2)/n)
op <- par(mfrow=c(3,2),mar=rep(1.5,4))
for(gamma in c(.4, .6, .8, 1, 1.2, 1.5))
  plot(y, axes = FALSE, frame.plot = TRUE,
       xlab = "", ylab = "", pch = 21, cex = 30,
       bg = rainbow(n, start=.85, end=.1, gamma = gamma),
       main = paste("Red tones;  gamma=",format(gamma)))
par(op)
}
\keyword{color}
\keyword{dplot}
