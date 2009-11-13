% File src/library/grid/man/unit.rep.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{raster}
\alias{raster}
\alias{is.raster}
\alias{as.raster}
\title{
  Create a raster object
}
\description{
  Functions to create a raster object (representing a bitmap
  image) and coerce other objects to a raster object.
}
\usage{
raster(x)
is.raster(x)
as.raster(x)
}
\arguments{
  \item{x}{
    Any R object.
  }
}
\details{
  It is not expected that the user will call these functions
  directly;  functions to render bitmap images
  in graphics packages will make use
  of the \code{as.raster()} function to automatically
  generate a raster object from their input.
  
  The \code{as.raster()} function is generic so methods can be
  written to convert other R objects to a raster object.
}
\value{
  For \code{raster()} and \code{as.raster()}, a raster object.

  For \code{is.raster()}, a logical indicating whether
  \code{x} is a raster object.
}
\examples{
# A red gradient
raster(matrix(hcl(0, 80, seq(50, 80, 10)),
              nrow=4, ncol=5))
}
\keyword{ dplot }
