% File src/library/grDevices/man/rgb2hsv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{rgb2hsv}
\alias{rgb2hsv}
\title{RGB to HSV Conversion}
\concept{color space conversion}
\description{
  \code{rgb2hsv} transforms colors
  from RGB space (red/green/blue)
  into HSV space (hue/saturation/value).
}
\usage{
rgb2hsv(r, g = NULL, b = NULL, maxColorValue = 255)
}
\arguments{
  \item{r}{vector of \sQuote{red} values in \eqn{[0, M]}, (\eqn{M
      = }\code{maxColorValue}) or 3-row rgb matrix.}
  \item{g}{vector of \sQuote{green} values, or \code{\link{NULL}} when
    \code{r} is a matrix.}
  \item{b}{vector of \sQuote{blue} values, or \code{\link{NULL}} when
    \code{r} is a matrix.}
  \item{maxColorValue}{number giving the maximum of the RGB color values
    range.  The default \code{255} corresponds to the typical \code{0:255}
    RGB coding as in \code{\link{col2rgb}()}.}
}
\details{
  Value (brightness) gives the amount of light in the color.\cr
  Hue describes the dominant wavelength.\cr
  Saturation is the amount of Hue mixed into the color.

  An HSV colorspace is relative to an RGB colorspace, which in \R
  is sRGB, which has an implicit gamma correction.
}
\value{
  A matrix with a column for each color.  The three rows of the matrix
  indicate hue, saturation and value and are named \code{"h"},
  \code{"s"}, and \code{"v"} accordingly.
}
\seealso{
  \code{\link{hsv}}, \code{\link{col2rgb}}, \code{\link{rgb}}.
}
\author{
  \R interface by Wolfram Fischer \email{wolfram@fischer-zim.ch};\cr
  C code mainly by Nicholas Lewin-Koh \email{nikko@hailmail.net}.
}
\examples{
## These (saturated, bright ones) only differ by hue
(rc <- col2rgb(c("red", "yellow","green","cyan", "blue", "magenta")))
(hc <- rgb2hsv(rc))
6 * hc["h",] # the hues are equispaced

\testonly{set.seed(151)}
(rgb3 <- floor(256 * matrix(stats::runif(3*12), 3, 12)))
(hsv3 <- rgb2hsv(rgb3))
## Consistency :
stopifnot(rgb3 == col2rgb(hsv(h = hsv3[1,], s = hsv3[2,], v = hsv3[3,])),
          all.equal(hsv3, rgb2hsv(rgb3/255, maxColorValue = 1)))

## A (simplified) pure R version -- originally by Wolfram Fischer --
## showing the exact algorithm:
rgb2hsvR <- function(rgb, gamma = 1, maxColorValue = 255)
{
    if(!is.numeric(rgb)) stop("rgb matrix must be numeric")
    d <- dim(rgb)
    if(d[1] != 3) stop("rgb matrix must have 3 rows")
    n <- d[2]
    if(n == 0) return(cbind(c(h = 1, s = 1, v = 1))[,0])
    rgb <- rgb/maxColorValue
    if(gamma != 1) rgb <- rgb ^ (1/gamma)

    ## get the max and min
    v <- apply( rgb, 2, max)
    s <- apply( rgb, 2, min)
    D <- v - s # range

    ## set hue to zero for undefined values (gray has no hue)
    h <- numeric(n)
    notgray <- ( s != v )

    ## blue hue
    idx <- (v == rgb[3,] & notgray )
    if (any (idx))
        h[idx] <- 2/3 + 1/6 * (rgb[1,idx] - rgb[2,idx]) / D[idx]
    ## green hue
    idx <- (v == rgb[2,] & notgray )
    if (any (idx))
        h[idx] <- 1/3 + 1/6 * (rgb[3,idx] - rgb[1,idx]) / D[idx]
    ## red hue
    idx <- (v == rgb[1,] & notgray )
    if (any (idx))
        h[idx] <-       1/6 * (rgb[2,idx] - rgb[3,idx]) / D[idx]

    ## correct for negative red
    idx <- (h < 0)
    h[idx] <- 1+h[idx]

    ## set the saturation
    s[! notgray] <- 0;
    s[notgray] <- 1 - s[notgray] / v[notgray]

    rbind( h = h, s = s, v = v )
}

## confirm the equivalence:
all.equal(rgb2hsv (rgb3),
          rgb2hsvR(rgb3), tolerance = 1e-14) # TRUE
}
\keyword{color}
\keyword{dplot}
