\name{quartz}
\alias{quartz}
\title{MacOS X Quartz device}
\description{
  \code{quartz} starts a graphics device driver for the MacOS X System.
  This can only be done on machines that run MacOS X.
}
\usage{
quartz(display = "", width = 5, height = 5, pointsize = 12,
       family = "Helvetica", antialias = TRUE, autorefresh = TRUE)
}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \env{DISPLAY}.}
  \item{width}{the width of the plotting window in inches.}
  \item{height}{the height of the plotting window in inches.}
  \item{pointsize}{the default pointsize to be used.}
  \item{family}{this is the family name of the Postscript font
    that will be used by the device.}
  \item{antialias}{whether to use antialiasing. It is never the case to set
     it \code{FALSE}}
  \item{autorefresh}{logical specifying if realtime refreshing should be
    done.  If \code{FALSE}, the system is charged to refresh the context
      of the device window.}
 }
\details{
  Quartz is the graphic engine based on the PDF format.  It is used
  by the graphic interface of MacOS X to render high quality graphics.
  As PDF it is device independent and can be rescaled without loss of
  definition.

  If a device-independent R graphics font family is specified
  (e.g., via \code{par(family=)} in the graphics package),
  the Quartz device makes use of the Quartz font database
  (see \code{quartzFonts}) to
  convert the R graphics font family
  to a Quartz-specific font family description.  
  
  Calling \code{quartz()} sets \code{\link{.Device}} to \code{"quartz"}.
}
\seealso{
  \code{\link{quartzFonts}},
  \code{\link{Devices}}.
}
\keyword{device}
