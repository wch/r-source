% File src/library/grDevices/man/dev.capabilities.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-12 R Core Team
% Distributed under GPL 2 or later

\name{dev.capabilities}
\alias{dev.capabilities}
\title{Query Capabilities of the Current Graphics Device}
\description{
  Query the capabilities of the current graphics device.
}
\usage{
dev.capabilities(what = NULL)
}
\arguments{
  \item{what}{a character vector partially matching the names of the
    components listed in section \sQuote{Value}, or \code{NULL} which
    lists all available capabilities.}
}
\details{
  The capabilities have to be specified by the author of the graphics
  device, unless they can be deduced from missing hooks.  Thus they will
  often by returned as \code{NA}, and may reflect the maximal
  capabilities of the underlying device where several output formats are
  supported by one device.

  Most recent devices support semi-transparent colours provided the
  graphics format does (which PostScript does not).  On the other hand,
  relatively few graphics formats support (fully or semi-) transparent
  backgrounds: generally the latter is found only in PDF and PNG plots.
}
\value{
  A named list with some or all of the following components, any of which may
  take value \code{NA}:
  \item{semiTransparency}{logical:  Does the device support
    semi-transparent colours?}
  \item{transparentBackground}{character:  Does the device support
    (semi)-transparent backgrounds?  Possible values are \code{"no"},
    \code{"fully"} (only full transparency) and \code{"semi"}
    (semi-transparent background colours are supported).}
  \item{rasterImage}{character: To what extent does the device support
    raster images as used by \code{\link{rasterImage}} and
    \code{\link{grid.raster}}?  Possible values \code{"no"}, \code{"yes"} and
    \code{"non-missing"} (support only for arrays without any missing
    values).}
  \item{capture}{logical:  Does the current device
    support raster capture as used by \code{\link{grid.cap}}?}
  \item{locator}{logical:  Does the current device
    support \code{\link{locator}} and \code{\link{identify}}?}
  \item{events}{character:  Which events can be generated on this device?
    Currently this will be a subset of \code{c("MouseDown",
    "MouseMove", "MouseUp", "Keybd")}, but other events may be
    supported in the future.  }
}
\seealso{
  See \code{\link{getGraphicsEvent}} for details on interactive events.
}
\examples{
dev.capabilities()
}
\keyword{ dplot }
