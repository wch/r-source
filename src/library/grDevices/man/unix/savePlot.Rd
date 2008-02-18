% File src/library/grDevices/man/windows/savePlot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{savePlot}
\alias{savePlot}
\title{Save Cairo X11 Plot to File.}
\description{
  Save the current page of a Cairo \code{\link{X11}()} device to a file. 
}
\usage{
savePlot(filename = "Rplot.png", type = "png", device = dev.cur())
}
\arguments{
  \item{filename}{filename to save to.}
  \item{type}{file type: currently only \code{png} is accepted.}
  \item{device}{the device to save from.}
}
\details{
  Only \code{X11} devices of type \code{type = "Cairo"} are supported.

  This works by using a cairo function to copy the backing image surface
  to a file.  This will always be a 24-bit per pixel PNG
  \sQuote{DirectClass} file, and transparency is not supported.

  If the current plot had a transparent or translucent background, the
  saved plot will have background colour the plot background
  alpha-blended with the device's canvas colour.
}
\value{
  Invisible \code{NULL}.
}
\seealso{
  \code{\link{X11}}, \code{\link{dev.print}}
}
\references{ ~put references to the literature/web site here ~ }
\keyword{misc}
