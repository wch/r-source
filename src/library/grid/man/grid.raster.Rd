% File src/library/grid/man/grid.record.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-2013 R Core Team
% Distributed under GPL 2 or later

\name{grid.raster}
\alias{grid.raster}
\alias{rasterGrob}
\title{
  Render a raster object
}
\description{
  Render a raster object (bitmap image) at the given location, size,
  and orientation.
}
\usage{
grid.raster(image,
            x = unit(0.5, "npc"), y = unit(0.5, "npc"),
            width = NULL, height = NULL,
            just = "centre", hjust = NULL, vjust = NULL,
            interpolate = TRUE, default.units = "npc",
            name = NULL, gp = gpar(), vp = NULL)

rasterGrob(image,
           x = unit(0.5, "npc"), y = unit(0.5, "npc"),
           width = NULL, height = NULL,
           just = "centre", hjust = NULL, vjust = NULL,
           interpolate = TRUE, default.units = "npc",
           name = NULL, gp = gpar(), vp = NULL)
}
\arguments{
  \item{image}{
    Any R object that can be coerced to a raster object.
  }
  \item{x}{A numeric vector or unit object specifying x-location.}
  \item{y}{A numeric vector or unit object specifying y-location.}
  \item{width}{A numeric vector or unit object specifying width.}
  \item{height}{A numeric vector or unit object specifying height.}
  \item{just}{The justification of the rectangle
    relative to its (x, y) location.  If there are two values, the first
    value specifies horizontal justification and the second value specifies
    vertical justification.  Possible string values are: \code{"left"},
    \code{"right"}, \code{"centre"}, \code{"center"}, \code{"bottom"},
    and \code{"top"}.  For numeric values, 0 means left alignment
    and 1 means right alignment.
  }
  \item{hjust}{A numeric vector specifying horizontal justification.
    If specified, overrides the \code{just} setting.}
  \item{vjust}{A numeric vector specifying vertical justification.
    If specified, overrides the \code{just} setting.}
  \item{default.units}{A string indicating the default units to use
    if \code{x}, \code{y}, \code{width}, or \code{height}
    are only given as numeric vectors.}
  \item{name}{ A character identifier. }
  \item{gp}{An object of class \code{gpar}, typically the output
    from a call to the function \code{gpar}.  This is basically
    a list of graphical parameter settings.}
  \item{vp}{A Grid viewport object (or NULL).}
  \item{interpolate}{
    A logical value indicating whether to linearly interpolate the
    image (the alternative is to use nearest-neighbour interpolation,
    which gives a more blocky result).
  }
}
\details{
  Neither \code{width} nor \code{height} needs to be specified,
  in which case, the aspect ratio of the image is preserved.  If
  both \code{width} and \code{height} are specified, it is likely
  that the image will be distorted.

  Not all graphics devices are capable of rendering raster images
  and some may not be able to produce rotated images (i.e., if
  a raster object is rendered within a rotated viewport).  See also the
  comments under \code{\link{rasterImage}}.

  All graphical parameter settings in \code{gp} will be ignored,
  including \code{alpha}.
}
\value{
  A rastergrob grob.
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{as.raster}}.

  \code{\link{dev.capabilities}} to see if it is supported.
}
\examples{
redGradient <- matrix(hcl(0, 80, seq(50, 80, 10)),
                      nrow=4, ncol=5)
# interpolated
grid.newpage()
grid.raster(redGradient)
# blocky
grid.newpage()
grid.raster(redGradient, interpolate=FALSE)
# blocky and stretched
grid.newpage()
grid.raster(redGradient, interpolate=FALSE, height=unit(1, "npc"))

# The same raster drawn several times
grid.newpage()
grid.raster(0, x=1:3/4, y=1:3/4, w=.1, interp=FALSE)
}
\keyword{ dplot }
