% File src/library/grDevices/man/windows/savePlot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{savePlot}
\alias{savePlot}
\title{Save Cairo X11 Plot to File}
\description{
  Save the current page of a cairo \code{\link{X11}()} device to a file. 
}
\usage{
savePlot(filename = paste("Rplot", type, sep="."),
         type = c("png", "jpeg", "tiff", "bmp"),
         device = dev.cur())
}
\arguments{
  \item{filename}{filename to save to.}
  \item{type}{file type: only \code{png} will be accepted for cairo
    version 1.0.}
  \item{device}{the device to save from.}
}
\details{
  Only \code{X11} devices of type \code{type = "Cairo"} are supported.

  This works by copying the backing image surface to a file.  For PNG
  will always be a 24-bit per pixel PNG \sQuote{DirectClass} file, for
  JPEG the quality is 75\% and for TIFF there is no compression.

  All transparency information is discarded: if the plot has
  \code{bg = "transparent"} (the default) this is regarded as opaque white.
}
\value{
  Invisible \code{NULL}.
}
\seealso{
  \code{\link{X11}}, \code{\link{dev.print}}
}
\keyword{utilities}
