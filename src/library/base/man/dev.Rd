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
\alias{.Device}
\alias{.Devices}
\alias{graphics.off}

\arguments{
  \item{which}{An integer specifying a device number}
}
\description{
  These functions provide control over multiple graphics devices.

  Only one device is the \emph{active} device.  This is the device in
  which all graphics operations occur.

  Devices are associated with a name (e.g., "X11"  or "postscript") and a
  number;  the "null device" is always device 1.

  \code{dev.off} shuts down the specified (by default the current) device.
  \code{graphics.off()} shuts down all open graphics devices.

  \code{dev.set} makes the specified device the active device.

  A list of device names is stored in \code{.Devices}.
  The name of the active device is stored in \code{.Device}.
}
\value{
\code{dev.cur} returns the number and name of the active device.

\code{dev.list} returns the numbers and names of all devices.

\code{dev.next} returns the number and name of the next device in the
list of devices.

\code{dev.prev} returns the number and name of the previous device in
the list of devices.

\code{dev.off} returns the name and number of the new active device
(after the specified device has been shut down).

\code{dev.set} returns the name and number of the new active device.
}
\seealso{
    \code{\link{Devices}}, such as \code{\link{postscript}}, etc;
    \code{\link{graphics.off}} for closing all devices;
    \code{\link{layout}} and its links for setting up plotting regions
    on the current device.
}
\examples{
\dontrun{
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
