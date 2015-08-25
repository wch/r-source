% File src/library/grDevices/man/dev.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{dev}
\alias{dev.cur}
\alias{dev.list}
\alias{dev.next}
\alias{dev.prev}
\alias{dev.off}
\alias{dev.set}
\alias{dev.new}
\alias{graphics.off}
\title{Control Multiple Devices}
\description{
  These functions provide control over multiple graphics devices.
}
\usage{
dev.cur()
dev.list()
dev.next(which = dev.cur())
dev.prev(which = dev.cur())
dev.off(which = dev.cur())
dev.set(which = dev.next())
dev.new(\dots, noRStudioGD = FALSE)
graphics.off()
}

\arguments{
  \item{which}{An integer specifying a device number.}
  \item{\dots}{arguments to be passed to the device selected.}
  \item{noRStudioGD}{Do not use the RStudio graphics device even if
    specified as the default device: it does not accept arguments such
    as \code{width} and \code{height}.}
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
  attempt to shut down device 1.  \code{graphics.off()} shuts down all
  open graphics devices.  Normal termination of a session runs the
  internal equivalent of \code{graphics.off()}.

  \code{dev.set} makes the specified device the active device.  If there
  is no device with that number, it is equivalent to \code{dev.next}.
  If \code{which = 1} it opens a new device and selects that.

  \code{dev.new} opens a new device.  Normally \R will open a new device
  automatically when needed, but this enables you to open further
  devices in a platform-independent way.  (For which device is used see
  \code{\link{getOption}("device")}.)  Note that care is needed with
  file-based devices such as \code{\link{pdf}} and
  \code{\link{postscript}} and in that case file names such as
  \file{Rplots.pdf}, \file{Rplots1.pdf}, \dots, \file{Rplots999.pdf} are
  tried in turn.  Only named arguments are passed to the device, and
  then only if they match the argument list of the device.  Even so,
  care is needed with the interpretation of e.g.\sspace{}\code{width}, and for
  the standard bitmap devices \code{units = "in", res = 72} is forced if
  neither is supplied but both \code{width} and \code{height} are.
}
\value{
  \code{dev.cur} returns a length-one named integer vector giving the
  number and name of the active device, or 1, the null device, if none
  is active.

  \code{dev.list} returns the numbers of all open devices, except device
  1, the null device.  This is a numeric vector with a
  \code{\link{names}} attribute giving the device names, or \code{NULL} is
  there is no open device.

  \code{dev.next} and \code{dev.prev} return the number and name of the
  next / previous device in the list of devices.  This will be the null
  device if and only if there are no open devices.

  \code{dev.off} returns the number and name of the new active device
  (after the specified device has been shut down).

  \code{dev.set} returns the number and name of the new active device.

  \code{dev.new} returns the return value of the device opened, usually
  invisible \code{NULL}.
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
abline(0, 1) # through the 1:10 points
dev.set(dev.next())
abline(h = 0, col = "gray") # for the residual plot
dev.set(dev.prev())
dev.off(); dev.off() #- close the two X devices
}
}
\keyword{device}
\keyword{iplot}
