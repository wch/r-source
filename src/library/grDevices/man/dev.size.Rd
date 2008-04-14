% File src/library/grDevices/man/devSet.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008 R Core Development Team
% Distributed under GPL 2 or later

\name{dev.size}
\alias{dev.size}
\title{ Find Size of Device Surface }
\description{
  Find the dimensions of the device surface of the current device.
}
\usage{
dev.size(units = c("in", "cm", "px"))
}
\arguments{
  \item{units}{the units in which to return the value -- inches, cm, or
    pixels (device units).}
}
\value{
  A two-element numeric vector giving width and height of the current
  device; a new device is opened if there is none, similarly to
  \code{\link{dev.new}()}.
}
\seealso{
  The size information in inches can be obtained by
  \code{\link[graphics]{par}("din")}, but this provides a way to access it
  independent of the graphics sub-system in use.
}
\examples{
dev.size("cm")
}
\keyword{ dplot }
