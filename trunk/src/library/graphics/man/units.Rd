% File src/library/graphics/man/units.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{units}
\alias{xinch}
\alias{yinch}
\alias{xyinch}
\title{Graphical Units}
\description{
  \code{xinch} and \code{yinch} convert the specified number of inches
  given as their arguments into the correct units for plotting with
  graphics functions.  Usually, this only makes sense when normal
  coordinates are used, i.e., \emph{no} \code{log} scale (see the
  \code{log} argument to \code{\link{par}}).

  \code{xyinch} does the same for a pair of numbers \code{xy},
  simultaneously.
}
\usage{
xinch(x = 1, warn.log = TRUE)
yinch(y = 1, warn.log = TRUE)
xyinch(xy = 1, warn.log = TRUE)
}
\arguments{
  \item{x, y}{numeric vector}
  \item{xy}{numeric of length 1 or 2.}
  \item{warn.log}{logical; if \code{TRUE}, a warning is printed in case
    of active log scale.}
}
\examples{
all(c(xinch(), yinch()) == xyinch()) # TRUE
xyinch()
xyinch #- to see that is really   delta{"usr"} / "pin"

## plot labels offset 0.12 inches to the right
## of plotted symbols in a plot
with(mtcars, {
    plot(mpg, disp, pch = 19, main = "Motor Trend Cars")
    text(mpg + xinch(0.12), disp, row.names(mtcars),
         adj = 0, cex = .7, col = "blue")
    })
}
\keyword{dplot}
