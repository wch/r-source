\name{stars}
\alias{stars}
\title{Star Plots and Segment Diagrams of Multivariate Data}
\usage{
stars(x, full = TRUE, scale = TRUE, radius = TRUE, 
      labels = dimnames(x)[[1]],
      locations = NULL, xlim = NULL, ylim = NULL, len = 1,
      colors = NULL, key.loc = NULL, key.labels = NULL, key.xpd = TRUE,
      draw.segments = FALSE,
      axes = FALSE, cex = 0.8, lwd = 0.25, \dots)
}
\arguments{
  \item{x}{matrix of data.  One segment plot will be produced for each
    row of the matrix.  Missing values (\code{NA}) are allowed, but they
    are treated as if they were 0.}
  \item{full}{logical flag: if \code{TRUE}, the segment plots will
    occupy a full circle.  Otherwise, they occupy the (upper) semicircle
    only.}
  \item{scale}{logical flag: if \code{TRUE}, the columns of the data
    matrix are scaled independently so that the maximum value in each
    column is 1 and the minimum is 0.  If \code{FALSE}, the presumption
    is that the data have been scaled by some other algorithm to the
    range \eqn{[0,1]}.}
  \item{radius}{logical flag: in \code{TRUE}, the radii corresponding to
    each variable in the data will be drawn.}
  \item{labels}{vector of character strings for labeling the plots.
    Unlike the S function \code{stars}, no attempt is made to construct
    labels if \code{labels = NULL}.}
  \item{locations}{two column matrix with the x and y coordinates used
    to place each of the segment plots.  If \code{locations} is
    \code{NULL} the segment plots will be placed in a rectangular grid.}
  \item{xlim}{vector with the range of x coordinates to plot. }
  \item{ylim}{vector with the range of y coordinates to plot.}
  \item{len}{scale factor for the length of radii or segments.}
  \item{colors}{vector of integers, each of which specifies  a color
    for one of the segments.  Ignored if \code{draw.segments = FALSE}}
  \item{key.loc}{vector with x and y coordinates of the unit key.}
  \item{key.labels}{vector of character strings for labeling the
    segments of the unit key.  If omitted, the second component of
    \code{dimnames(x)} is used, if available.}
  \item{key.xpd}{clipping switch for the unit key (drawing and
    labeling), see \code{\link{par}("xpd")}.}
  \item{draw.segments}{logical. If \code{TRUE} draw a segment diagram.}
  \item{axes}{logical flag: if \code{TRUE} axes are added to the
    plot.}
  \item{cex}{character expansion factor for the labels.}
  \item{lwd}{line width used for drawing.}
  \item{\dots}{further arguments, passed to the first call of
    \code{plot()}, see \code{\link{plot.default}}.}
  }
}
\description{
  Star plots or segment diagrams are created on the current graphics
  device.
}
\details{
  Missing values are treated as 0.

  Each star plot or segment diagram represents one row of the input
  \code{x}.  Variables (columns) start on the right and wind
  counterclockwise around the circle.  The size of the (scaled) column
  is shown by the distance from the center to the point on the star or
  the radius of the segment representing the variable.

  Only one page of output is produced.
}
\author{Thomas S. Dye}
\note{
  This code started life as spatial star plots by David A. Andrews.
  See \url{http://www.udallas.edu:8080/~andrews/software/software.html}.
}
\examples{
data(mtcars)
stars(mtcars[, 1:7], key.loc = c(12, 2),
      main = "Motor Trend Cars", full = FALSE) 

stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 1.5),
      main = "Motor Trend Cars", draw.segments = TRUE)

data(USJudgeRatings)
stars(USJudgeRatings, labels = abbreviate(case.names(USJudgeRatings)),
      key.loc = c(13, 1.5), main = "Judge not ...", len = 0.8)
}
\keyword{hplot}
