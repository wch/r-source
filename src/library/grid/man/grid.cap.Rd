% File src/library/grid/man/grid.cap.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
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
  A matrix of R colour names.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{grid.raster}}

  \code{\link{dev.capabilities}} to see if it is supported.
}
\examples{
\dontrun{
dev.new(width=.5, height=.5)
grid.rect()
grid.text("hi")
cap <- grid.cap()
dev.off()

grid.raster(cap, width=.5, height=.5, interpolate=FALSE)
}
}
\keyword{ dplot }
