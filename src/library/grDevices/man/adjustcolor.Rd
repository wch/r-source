% File src/library/grDevices/man/palette.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2010 R Core Team
% Copyright 2002-2010 The R Foundation
% Distributed under GPL 2 or later

\name{adjustcolor}
\alias{adjustcolor}
\title{Adjust Colors in One or More Directions Conveniently.}
\description{
  Adjust or modify a vector of colors by \dQuote{turning knobs} on one or more
  coordinates in \eqn{(r,g,b,\alpha)} space, typically by up or down
  scaling them.
}

\usage{
adjustcolor(col, alpha.f = 1, red.f = 1, green.f = 1, blue.f = 1,
            offset = c(0, 0, 0, 0),
            transform = diag(c(red.f, green.f, blue.f, alpha.f)))}
\arguments{
  \item{col}{vector of colors, in any format that col2rgb() accepts}
  \item{alpha.f}{factor modifying the opacity alpha; typically in [0,1]}
  \item{red.f, green.f, blue.f}{factors modifying the \dQuote{red-},
    \dQuote{green-} or \dQuote{blue-}ness of the colors, respectively.}
  \item{offset}{
  }
  \item{transform}{
  }
}
\value{a color vector of the same length as \code{col}, effectively the
  result of \code{\link{rgb}()}.
}
%\author{Thomas Lumley, Luke Tierney, Martin Maechler, Duncan Murdoch...}
% \details{

% }
\seealso{
  \code{\link{rgb}}, \code{\link{col2rgb}}.  For more sophisticated
  color constructions: \code{\link{convertColor}}
}
\examples{
## Illustrative examples :
opal <- palette("default")
stopifnot(identical(adjustcolor(1:8,       0.75),
                    adjustcolor(palette(), 0.75)))
cbind(palette(), adjustcolor(1:8, 0.75))

##  alpha = 1/2 * previous alpha --> opaque colors
x <- palette(adjustcolor(palette(), 0.5))

sines <- outer(1:20, 1:4, function(x, y) sin(x / 20 * pi * y))
matplot(sines, type = "b", pch = 21:23, col = 2:5, bg = 2:5,
        main = "Using an 'opaque ('translucent') color palette")

x. <- adjustcolor(x, offset = c(0.5, 0.5, 0.5, 0), # <- "more white"
                  transform = diag(c(.7, .7, .7, 0.6)))
cbind(x, x.)
op <- par(bg = adjustcolor("goldenrod", offset = -rep(.4, 4)), xpd = NA)
plot(0:9, 0:9, type = "n", axes = FALSE, xlab = "", ylab = "",
     main = "adjustcolor() -> translucent")
text(1:8, labels = paste0(x,"++"), col = x., cex = 8)
par(op)

## and

(M <- cbind( rbind( matrix(1/3, 3, 3), 0), c(0, 0, 0, 1)))
adjustcolor(x, transform = M)

## revert to previous palette: active
palette(opal)
}
