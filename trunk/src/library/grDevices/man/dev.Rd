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
}
\details{
  Only one device is the \sQuote{active} device: this is the device in
  which all graphics operations occur.  There is a \code{"null device"}
  which is always open but is really a placeholder: any attempt to use
  it will open a new device specified by
  \code{\link{getOption}("device")}).

  Devices are associated with a name (e.g., \code{"X11"} or
  \code{"postscript"}) and a number in the range 1 to 63; the
  \code{"null device"} is always device 1.  Once a device has been
  opened the null device is not considered as a possible active device.
  There is a list of open devices, and this is considered as a circular
  list not including the null device.  \code{dev.next} and
  \code{dev.prev} select the next open device in the appropriate
  direction, unless no device is open.
  
  \code{dev.off} shuts down the specified (by default the current)
  device.  If the current device is shut down and any other devices are
  open, the next open device is made current.  It is an error to
  attempt to shut down device 1.
  \code{graphics.off()} shuts down all open graphics devices.

  \code{dev.set} makes the specified device the active device.  If there
  is no device with that number, it is equivalent to \code{dev.next}.
  If \code{which = 1} it opens a new device and selects that.
}
\value{
  \code{dev.cur} returns the number and name of the active device,
  or 1, the null device, if none is active.

  \code{dev.list} returns the numbers of all open devices, except device
  1, the null device.  This is a numeric vector with a names attribute
  giving the names, or \code{NULL} is there is no open device.

  \code{dev.next} and \code{dev.prev} return the number and name of the
  next / previous device in the list of devices.  This will be the null
  device if and only if there are no open devices.

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
