% File src/library/grid/man/as.raster.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{as.raster}
\alias{is.raster}
\alias{as.raster}
\alias{as.raster.logical}
\alias{as.raster.numeric}
\alias{as.raster.character}
\alias{as.raster.matrix}
\alias{as.raster.array}
\title{Create a Raster Object}
\description{
  Functions to create a raster object (representing a bitmap
  image) and coerce other objects to a raster object.
}
\usage{
is.raster(x)
as.raster(x, \dots)

\method{as.raster}{logical}(x, max=1, \dots)
\method{as.raster}{numeric}(x, max=1, \dots)
\method{as.raster}{character}(x, max=1, \dots)
\method{as.raster}{matrix}(x, max=1, \dots)
\method{as.raster}{array}(x, max=1, \dots)
}
\arguments{
  \item{x}{
    Any R object.
  }
  \item{max}{number giving the maximum of the color values range.}
  \item{\dots}{further arguments passed to or from other methods.}
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
  For \code{as.raster()}, a raster object.

  For \code{is.raster()}, a logical indicating whether
  \code{x} is a raster object.
}
\examples{
# A red gradient
as.raster(matrix(hcl(0, 80, seq(50, 80, 10)),
                 nrow=4, ncol=5))

# Vectors are 1-column matrices ...
#   character vectors are color names ...
as.raster(hcl(0, 80, seq(50, 80, 10)))
#   numeric vectors are greyscale ...
as.raster(1:5, max=5)
#   locigal vectors are black and white ...
as.raster(1:10 \%\% 2 == 0)

# ... unless nrow/ncol are supplied ...
as.raster(1:10 \%\% 2 == 0, nrow=1)

# Matrix can also be logical or numeric ...
as.raster(matrix(c(TRUE, FALSE), nrow=3, ncol=2))
as.raster(matrix(1:3/4, nrow=3, ncol=4))

# An array can be 3-plane numeric (R, G, B planes) ...
as.raster(array(c(0:1, rep(0.5, 4)), c(2, 1, 3)))

# ... or 4-plane numeric (R, G, B, A planes)
as.raster(array(c(0:1, rep(0.5, 6)), c(2, 1, 4)))

}
\keyword{ dplot }
