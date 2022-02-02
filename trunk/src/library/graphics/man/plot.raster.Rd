% File src/library/graphics/man/plotraster.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{plot.raster}
\title{Plotting Raster Images}
\usage{
\method{plot}{raster}(x, y,
     xlim = c(0, ncol(x)), ylim = c(0, nrow(x)),
     xaxs = "i", yaxs = "i",
     asp = 1, add = FALSE, \dots)
}
\alias{plot.raster}
\arguments{
  \item{x, y}{raster. \code{y} will be ignored.}
  \item{xlim, ylim}{Limits on the plot region (default from dimensions
    of the raster).}
  \item{xaxs, yaxs}{Axis interval calculation style (default means that
    raster fills plot region).}
  \item{asp}{Aspect ratio (default retains aspect ratio of the raster).}
  \item{add}{Logical indicating whether to simply add raster to an
    existing plot.}
  \item{\dots}{Further arguments to the \code{\link{rasterImage}} function.}
}
\description{
  This functions implements a \code{\link{plot}} method for raster images.
}
\seealso{
    \code{\link{plot.default}}, \code{\link{rasterImage}}.
}
\examples{
require(grDevices)
r <- as.raster(c(0.5, 1, 0.5))
plot(r)
# additional arguments to rasterImage()
plot(r, interpolate=FALSE)
# distort
plot(r, asp=NA)
# fill page
op <- par(mar=rep(0, 4))
plot(r, asp=NA)
par(op)
# normal annotations work
plot(r, asp=NA)
box()
title(main="This is my raster")
# add to existing plot
plot(1)
plot(r, add=TRUE)
}
\keyword{hplot}
