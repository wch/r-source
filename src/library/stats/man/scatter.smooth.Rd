% File src/library/stats/man/scatter.smooth.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{scatter.smooth}
\title{Scatter Plot with Smooth Curve Fitted by Loess}
\alias{scatter.smooth}
\alias{loess.smooth}
\description{
  Plot and add a smooth curve computed by \code{loess} to a scatter plot.
}
\usage{
scatter.smooth(x, y = NULL, span = 2/3, degree = 1,
    family = c("symmetric", "gaussian"),
    xlab = NULL, ylab = NULL,
    ylim = range(y, pred$y, na.rm = TRUE),
    evaluation = 50, \dots, lpars = list())

loess.smooth(x, y, span = 2/3, degree = 1,
    family = c("symmetric", "gaussian"), evaluation = 50, \dots)
}
\arguments{
  \item{x, y}{the \code{x} and \code{y} arguments provide the x and y
    coordinates for the plot.  Any reasonable way of defining the
    coordinates is acceptable.  See the function \code{\link{xy.coords}}
    for details.}
  \item{span}{smoothness parameter for \code{loess}.}
  \item{degree}{degree of local polynomial used.}
  \item{family}{if \code{"gaussian"} fitting is by least-squares, and if
    \code{family = "symmetric"} a re-descending M estimator is used.
    Can be abbreviated.}
  \item{xlab}{label for x axis.}
  \item{ylab}{label for y axis.}
  \item{ylim}{the y limits of the plot.}
  \item{evaluation}{number of points at which to evaluate the smooth
    curve.}
  \item{\dots}{For \code{scatter.smooth()}, graphical parameters, passed
    to \code{plot()} only.  For \code{loess.smooth}, control parameters
    passed to \code{\link{loess.control}}.}
  \item{lpars}{a \code{\link{list}} of arguments to be passed to
    \code{\link{lines}()}.}
}
\details{
  \code{loess.smooth} is an auxiliary function which evaluates the
  \code{loess} smooth at \code{evaluation} equally spaced points
  covering the range of \code{x}.
}
\value{
  For \code{scatter.smooth}, none.

  For \code{loess.smooth}, a list with two components, \code{x} (the
  grid of evaluation points) and \code{y} (the smoothed values at the
  grid points).
}
\seealso{
  \code{\link{loess}}; \code{\link{smoothScatter}} for scatter plots
  with smoothed \emph{density} color representation.
}
\examples{
require(graphics)

with(cars, scatter.smooth(speed, dist))
## or with dotted thick smoothed line results :
with(cars, scatter.smooth(speed, dist, lpars =
                    list(col = "red", lwd = 3, lty = 3)))
}
\keyword{smooth}
