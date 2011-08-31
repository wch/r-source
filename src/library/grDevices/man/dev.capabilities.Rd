% File src/library/grDevices/man/dev.capabilities.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008 R Core Development Team
% Distributed under GPL 2 or later

\name{dev.capabilities}
\alias{dev.capabilities}
\title{Query Capabilities of the Current Graphics Device}
\description{
  Query the capabilibities of the current graphics device.
}
\usage{
dev.capabilities(what = NULL)
}
\arguments{
  \item{what}{a character vector including names of the components
    listed in section \sQuote{Value}, or \code{NULL} which lists all
    available capabilities.}
}
\details{
  The capabilities have to be specified by the author of the graphics
  device, unless they can be deduced from missing hooks.  Thus they will
  often by returned as \code{NA}, and may reflect the maximal
  capabilities of the device.
}
\value{
  A list with some of all of the following components, any of which may
  be \code{NA}:
  \item{transparency}{character:  To what extend does the device support
    (semi)-transparent colours?  Possible values as \code{"none"},
    \code{"bg"} (fully transparent backgrounds), \code{"fully"} (fully
    transparent colours) and \code{"alpha"} (full support for
    semi-transparent colours, including backgrounds).}
  \item{rasterImage}{character: To what extent does the device support
    raster images as used by \code{\link{rasterImage}} and
    \code{\link{grid.raster}}?  Possible values \code{"no"}, \code{"yes"} and
    \code{"non-missing"} (support only for arrays without any missing
    values).}  
  \item{capture}{logical:  Does the current device
    support raster capture as used by \code{\link{grid.cap}}?}
  \item{locator}{logical:  Does the current device
    support \code{\link{locator}} and \code{\link{identify}}?}
}
\examples{
dev.capabilities()

dev.capabilities("transparency")[[1L]] \%in\% c("fully", "alpha")
}
\keyword{ dplot }
