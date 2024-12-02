% File src/library/grDevices/man/msgWindow.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2024 R Core Team
% Distributed under GPL 2 or later

\title{Manipulate a Window}
\name{msgWindow}
\alias{msgWindow}
\usage{
msgWindow(type = c("minimize", "restore", "maximize",
                   "hide", "recordOn", "recordOff"),
          which = dev.cur())
}
\arguments{
  \item{type}{action to be taken.}
  \item{which}{a device number, or \code{-1}.}
}
\description{
  On Windows, only,
  \code{msgWindow} sends a message to manipulate the specified screen
  device's window.  With argument \code{which = -1} it applies to the GUI
  console (which only accepts the first three actions).
}
\seealso{
  \code{\link{bringToTop}}, \code{\link{windows}}
}
\keyword{device}
