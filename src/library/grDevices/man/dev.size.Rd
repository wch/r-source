% File src/library/grDevices/man/devSet.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2008 R Core Development Team
% Distributed under GPL 2 or later

\name{dev.size}
\alias{dev.size}
\title{ Find Size of Device Surface }
\description{
  This function can be used to control (for the current device)
  whether the user is prompted before starting a new page of output.
}
\usage{
dev.size(units = c("in", "px"))
}
\arguments{
  \item{units}{the units in which to return the value -- inches of
    pixels (device units).}
}
\value{
  A two-element numeric vector giving width and height of the current
  device.  It is an error to request the size of the null device.
}

\keyword{ dplot }
