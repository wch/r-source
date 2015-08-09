% File src/library/grid/man/grid.cap.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{grid.cap}
\alias{grid.cap}
\title{
  Capture a raster image
}
\description{
  Capture the current contents of a graphics device as a raster
  (bitmap) image.
}
\usage{
grid.cap()
}
\details{
  This function is only implemented for on-screen graphics devices.
}
\value{
  A matrix of R colour names, or \code{NULL} if not available.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.raster}}

  \code{\link{dev.capabilities}} to see if it is supported.
}
\examples{
\donttest{
dev.new(width=0.5, height=0.5)
grid.rect()
grid.text("hi")
cap <- grid.cap()
dev.off()

if(!is.null(cap))
    grid.raster(cap, width=0.5, height=0.5, interpolate=FALSE)
}
}
\keyword{ dplot }
