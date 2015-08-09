% File src/library/grDevices/man/hsv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{hsv}
\alias{hsv}
\title{HSV Color Specification}
\description{
  Create a vector of colors from vectors specifying hue, saturation and
  value.
}
\usage{
hsv(h = 1, s = 1, v = 1, alpha)
}
\arguments{
  \item{h,s,v}{numeric vectors of values in the range \code{[0, 1]} for
    \sQuote{hue}, \sQuote{saturation} and \sQuote{value} to be combined
    to form a vector of colors.  Values in shorter arguments are
    recycled.}
  \item{alpha}{numeric vector of values in the range \code{[0, 1]} for
    alpha transparency channel (0 means transparent and 1 means opaque).}
}
\details{
  Semi-transparent colors (\code{0 < alpha < 1}) are supported only on
  some devices: see \code{\link{rgb}}.
}
\value{
  This function creates a vector of colors corresponding to the
  given values in HSV space.  The values returned by \code{hsv} can be
  used with a \code{col=} specification in graphics functions or in
  \code{par}.
}
\seealso{
  \code{\link{hcl}} for a perceptually based version of \code{hsv()},
  \code{\link{rgb}} and \code{\link{rgb2hsv}} for RGB to HSV conversion;
  \code{\link{rainbow}},
  \code{\link{gray}}.
}
\examples{
require(graphics)

hsv(.5,.5,.5)

## Red tones:
n <- 20;  y <- -sin(3*pi*((1:n)-1/2)/n)
op <- par(mar = rep(1.5, 4))
plot(y, axes = FALSE, frame.plot = TRUE,
     xlab = "", ylab = "", pch = 21, cex = 30,
     bg = rainbow(n, start = .85, end = .1),
     main = "Red tones")
par(op)
}
\keyword{color}
\keyword{dplot}
