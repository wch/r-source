\name{dev.xxx}
\title{Control Multiple Devices}
\usage{
dev.cur()
dev.list()
dev.next(which = dev.cur())
dev.prev(which = dev.cur())
dev.off(which = dev.cur())
dev.set(which = dev.next())
graphics.off()
}
\alias{dev.cur}
\alias{dev.list}
\alias{dev.next}
\alias{dev.prev}
\alias{dev.off}
\alias{dev.set}
\alias{graphics.off}

\arguments{
  \item{which}{An integer specifying a device number}
}
\description{
  These functions provide control over multiple graphics devices.

  Only one device is the \emph{active} device.  This is the device in
  which all graphics operations occur.

  Devices are associated with a name (e.g., \code{"X11"} or
  \code{"postscript"}) and a number; the \code{"null device"} is always
  device 1.

  \code{dev.off} shuts down the specified (by default the current) device.
  \code{graphics.off()} shuts down all open graphics devices.

  \code{dev.set} makes the specified device the active device.
}
\value{
  \code{dev.cur} returns the number and name of the active device,
  or 1, the null device, if none is active.

  \code{dev.list} returns the numbers of all open devices, except device
  1, the null device.  This is a numeric vector with a names attribute
  giving the names, or \code{NULL} is there is no open device.

  \code{dev.next} and \code{dev.prev} return the number and name of the
  next / previous device in the list of devices.  The list is regarded as
  a circular list, and \code{"null device"} will be included only if there
  are no open devices.

  \code{dev.off} returns the name and number of the new active device
  (after the specified device has been shut down).

  \code{dev.set} returns the name and number of the new active device.
}
\seealso{
  \code{\link{Devices}}, such as \code{\link{postscript}}, etc.

  \code{\link{layout}} and its links for setting up plotting regions
  on the current device.
}
\examples{
\dontrun{## Unix-specific example
x11()
plot(1:10)
x11()
plot(rnorm(10))
dev.set(dev.prev())
abline(0,1)# through the 1:10 points
dev.set(dev.next())
abline(h=0, col="gray")# for the residual plot
dev.set(dev.prev())
dev.off(); dev.off()#- close the two X devices
}
}
\keyword{device}
\keyword{iplot}
