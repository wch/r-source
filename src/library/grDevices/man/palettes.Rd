\name{Palettes}
\alias{rainbow}
\alias{heat.colors}
\alias{terrain.colors}
\alias{topo.colors}
\alias{cm.colors}
\title{Color Palettes}
\description{
  Create a vector of \code{n} \dQuote{contiguous} colors.
}
\usage{
rainbow(n, s = 1, v = 1, start = 0, end = max(1,n - 1)/n, gamma = 1)
heat.colors(n)
terrain.colors(n)
topo.colors(n)
cm.colors(n)
}
\arguments{
  \item{n}{the number of colors (\eqn{\ge 1}{>= 1}) to be in the
    palette.}
  \item{s,v}{the \dQuote{saturation} and \dQuote{value} to be used to complete the
    HSV color descriptions.}
  \item{start}{the (corrected) hue in [0,1] at which the rainbow
    begins.}
  \item{end}{the (corrected) hue in [0,1] at which the rainbow ends.}
  \item{gamma}{the gamma correction, see argument \code{gamma} in
    \code{\link{hsv}}.}
}
\details{
  Conceptually, all of these functions actually use (parts of) a line
  cut out of the 3-dimensional color space, parametrized by
  \code{\link{hsv}(h,s,v, gamma)}, where \code{gamma}\eqn{=1} for the
  \emph{foo}\code{.colors} function, and hence,
  equispaced hues in RGB space tend to cluster at
  the red, green and blue primaries.

  Some applications such as contouring require a palette
  of colors which do not \dQuote{wrap around} to give a final
  color close to the starting one.

  With \code{rainbow}, the parameters \code{start} and \code{end} can be used
  to specify particular subranges of hues.
  The following values can be used when generating such a subrange:
  red=0, yellow=\eqn{\frac 1 6}{1/6}, green=\eqn{\frac 2 6}{2/6},
  cyan=\eqn{\frac 3 6}{3/6}, blue=\eqn{\frac 4 6}{4/6}
  and magenta=\eqn{\frac 5 6}{5/6}.
}
\value{A character vector, \code{cv}, of color names.  This can be used
  either to create a user--defined color palette for subsequent
  graphics by \code{\link{palette}(cv)}, a \code{col=} specification
  in graphics functions or in \code{par}.
}
\seealso{
  \code{\link{colors}}, \code{\link{palette}},  \code{\link{hsv}},
  \code{\link{rgb}}, \code{\link{gray}} and \code{\link{col2rgb}} for
  translating to RGB numbers.
}
\examples{
require(graphics)
# A Color Wheel
pie(rep(1,12), col=rainbow(12))

##------ Some palettes ------------
demo.pal <-
  function(n, border = if (n<32) "light gray" else NA,
           main = paste("color palettes;  n=",n),
           ch.col = c("rainbow(n, start=.7, end=.1)", "heat.colors(n)",
                      "terrain.colors(n)", "topo.colors(n)", "cm.colors(n)"))
{
    nt <- length(ch.col)
    i <- 1:n; j <- n / nt; d <- j/6; dy <- 2*d
    plot(i,i+d, type="n", yaxt="n", ylab="", main=main)
    for (k in 1:nt) {
        rect(i-.5, (k-1)*j+ dy, i+.4, k*j,
             col = eval(parse(text=ch.col[k])), border = border)
        text(2*j,  k * j +dy/4, ch.col[k])
    }
}
n <- if(.Device == "postscript") 64 else 16
     # Since for screen, larger n may give color allocation problem
demo.pal(n)
}
\keyword{color}
\keyword{dplot}
