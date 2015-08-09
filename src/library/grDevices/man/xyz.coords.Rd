% File src/library/grDevices/man/xyz.coords.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{xyz.coords}
\alias{xyz.coords}
\title{Extracting Plotting Structures}
\description{
  Utility for obtaining consistent x, y and z coordinates and labels
  for three dimensional (3D) plots.
}
\usage{
xyz.coords(x, y = NULL, z = NULL,
           xlab = NULL, ylab = NULL, zlab = NULL,
           log = NULL, recycle = FALSE)
}
\arguments{
  \item{x, y, z}{the x, y and z coordinates of a set of points.
    Both \code{y} and \code{z} can be left at \code{NULL}.  In this
    case, an attempt is made to interpret \code{x} in a way suitable for
    plotting.

    If the argument is a formula \code{zvar ~ xvar + yvar},
    \code{xvar}, \code{yvar} and \code{zvar} are used as x, y and z
    variables;
    if the argument is a list containing components \code{x},
    \code{y} and \code{z}, these are assumed to define plotting coordinates;
    if the argument is a matrix or \code{\link{data.frame}} with three
    or more columns, the first is
    assumed to contain the x values, the 2nd the y ones, and the 3rd the
    z ones -- independently of any column names that \code{x} may have.

    Alternatively two arguments \code{x} and \code{y} can be provided
    (leaving \code{z = NULL}).  One may be real, the other complex;
    in any other case, the arguments are coerced to vectors
    and the values plotted against their indices.
  }
  \item{xlab, ylab, zlab}{names for the x, y and z variables to be extracted.}
  \item{log}{character, \code{"x"}, \code{"y"}, \code{"z"} or combinations.
    Sets negative values to \code{\link{NA}} and gives a warning.}
  \item{recycle}{logical; if \code{TRUE}, recycle (\code{\link{rep}}) the
    shorter ones of \code{x}, \code{y} or \code{z} if their lengths differ.}
}
\value{
  A list with the components
  \item{x}{numeric (i.e., \code{\link{double}}) vector of abscissa values.}
  \item{y}{numeric vector of the same length as \code{x}.}
  \item{z}{numeric vector of the same length as \code{x}.}
  \item{xlab}{\code{character(1)} or \code{NULL}, the axis label of \code{x}.}
  \item{ylab}{\code{character(1)} or \code{NULL}, the axis label of \code{y}.}
  \item{zlab}{\code{character(1)} or \code{NULL}, the axis label of \code{z}.}
}
\author{Uwe Ligges and Martin Maechler}
\seealso{\code{\link{xy.coords}} for 2D.}
\examples{
xyz.coords(data.frame(10*1:9, -4), y = NULL, z = NULL)

xyz.coords(1:5, stats::fft(1:5), z = NULL, xlab = "X", ylab = "Y")

y <- 2 * (x2 <- 10 + (x1 <- 1:10))
xyz.coords(y ~ x1 + x2, y = NULL, z = NULL)

xyz.coords(data.frame(x = -1:9, y = 2:12, z = 3:13), y = NULL, z = NULL,
           log = "xy")
##> Warning message: 2 x values <= 0 omitted ...
}
\keyword{dplot}
