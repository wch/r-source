% File src/library/grDevices/man/dev.capture.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011 R Core Team
% Distributed under GPL 2 or later

\name{dev.capture}
\alias{dev.capture}
\title{
  Capture device output as a raster image
}
\description{
  \code{dev.capture} captures the current contents of a graphics device
  as a raster (bitmap) image.
}
\usage{
dev.capture(native = FALSE)
}
\arguments{
  \item{native}{Logical. If \code{FALSE} the result is a matrix of R
    color names, if \code{TRUE} the output is returned as a
    \code{nativeRaster} object which is more efficient for plotting, but
    not portable.}
}
\details{
  Not all devices support capture of the output as raster bitmaps.
  Typically, only image-based devices do and even not all of them.
}
\value{
  \code{NULL} if the device does not support capture, otherwise a
  matrix of color names (for \code{native = FALSE}) or a
  \code{nativeRaster} object (for \code{native = TRUE}).
}
\keyword{ dplot }
