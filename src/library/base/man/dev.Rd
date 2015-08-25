% File src/library/base/man/dev.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{.Device}
\alias{.Device}
\alias{.Devices}
\title{
  Lists of Open/Active Graphics Devices
}
\description{
  A pairlist of the names of open graphics devices is stored in
  \code{.Devices}. The name of the active device (see
  \code{\link{dev.cur}}) is stored in \code{.Device}.  Both are symbols
  and so appear in the base namespace.
}
\value{
  \code{.Device} is a length-one character vector.

  \code{.Devices} is a \link{pairlist} of length-one character vectors.
  The first entry is always \code{"null device"}, and there are as many
  entries as the maximal number of graphics devices which have been
  simultaneously active.  If a device has been removed, its entry will be
  \code{""} until the device number is reused.

  Devices may add attributes to the character vector: for example
  devices which write to a file may record its path in attribute
  \code{"filepath"}.
}
\keyword{device}
