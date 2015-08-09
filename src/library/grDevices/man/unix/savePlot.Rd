% File src/library/grDevices/man/windows/savePlot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-9 R Core Team
% Distributed under GPL 2 or later

\name{savePlot}
\alias{savePlot}
\title{Save Cairo X11 Plot to File}
\description{
  Save the current page of a cairo \code{\link{X11}()} device to a file.
}
\usage{
savePlot(filename = paste("Rplot", type, sep = "."),
         type = c("png", "jpeg", "tiff", "bmp"),
         device = dev.cur())
}
\arguments{
  \item{filename}{filename to save to.}
  \item{type}{file type: only \code{"png"} will be accepted for cairo
    version 1.0.}
  \item{device}{the device to save from.}
}
\details{
  Only cairo-based \code{X11} devices are supported.

  This works by copying the image surface to a file.  For PNG
  will always be a 24-bit per pixel PNG \sQuote{DirectClass} file, for
  JPEG the quality is 75\% and for TIFF there is no compression.

  For devices with buffering this copies the buffer's image surface, so
  works even if \code{\link{dev.hold}} has been called.

  At present the plot is saved after rendering onto the canvas (default
  opaque white), so for the default \code{bg = "transparent"} the
  effective background colour is the canvas colour.
}
\value{
  Invisible \code{NULL}.
}

\note{
  There is a similar function of the same name but more types for
  \code{windows} devices on Windows.
}

\seealso{
  \code{\link{X11}}, \code{\link{dev.copy}}, \code{\link{dev.print}}
}
\keyword{utilities}
