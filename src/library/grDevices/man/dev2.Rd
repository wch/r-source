\name{dev2}
\alias{dev.copy}
\alias{dev.print}
\alias{dev.copy2eps}
\alias{dev.control}
\title{Copy Graphics Between Multiple Devices}
\usage{
dev.copy(device, \dots, which = dev.next())
dev.print(device = postscript, \dots)
dev.copy2eps(\dots)
dev.control(displaylist = c("inhibit", "enable"))
}
\arguments{
  \item{device}{A device function (e.g., \code{x11},
    \code{postscript}, \ldots)}
  \item{\dots}{Arguments to the \code{device} function above.
    For \code{dev.print}, this includes \code{which} and by default any
    \code{\link{postscript}} arguments.}
  \item{which}{A device number specifying the device to copy to}
  \item{displaylist}{A character string: the only valid values are
    \code{"inhibit"} and \code{"enable"}.}
}
\description{
  \code{dev.copy} copies the graphics contents of the current device
  to the device specified by \code{which} or to a new device which has
  been created by the function specified by \code{device} (it is an
  error to specify both \code{which} and \code{device}).  (If recording
  is off on the current device, there are no contents to copy: this will
  result in no plot or an empty plot.)
  The device copied to becomes the current device.

  \code{dev.print} copies the graphics contents of the current device
  to a new device which has been created by the function specified by
  \code{device} and then shuts the new device.

  \code{dev.copy2eps} is similar to \code{dev.print} but produces an
  EPSF output file, in portrait orientation (\code{horizontal = FALSE})

  \code{dev.control} allows the user to control the recording of
  graphics operations in a device.  If \code{displaylist} is \code{"inhibit"}
  (\code{"enable")} then recording is turned off (on). It is only safe
  to change this at the beginning of a plot (just before or just after a
  new page).  Initially recording is on for screen devices, and off for
  print devices.
}
\value{
  \code{dev.copy} returns the name and number of the device which has
  been copied to.

  \code{dev.print} and \code{dev.copy2eps} return the name and number of
  the device which has been copied from.
}
\details{
  For \code{dev.copy2eps}, \code{width} and \code{height} are taken
  from the current device unless otherwise specified.  If just one of
  \code{width} and \code{height} is specified, the other is adjusted
  to preserve the aspect ratio of the device being copied.  The default
  file name is \code{Rplot.eps}.

#ifdef unix
  The default for \code{dev.print} is to produce and print a postscript
  copy, if \code{\link{options}("printcmd")} is set suitably.
#endif
#ifdef windows
  The default for \code{dev.print} is to produce and print a postscript
  copy.  This will not work unless
  \code{\link{options}("printcmd")} is set suitably and you have a
  PostScript printer: see \code{\link{postscript}} for how to set this
  up.  Windows users may prefer to use \code{dev.print(win.print)}.
#endif


  \code{dev.print} is most useful for producing a postscript print
  (its default) when the following applies.  Unless
  \code{file} is specified, the plot will be printed.  Unless
  \code{width}, \code{height} and \code{pointsize} are specified
  the plot dimensions will be taken from the current device, shrunk
  if necessary to fit on the paper. (\code{pointsize} is rescaled if the
  plot is shrunk.)  If \code{horizontal} is not specified and the plot
  can be printed at full size by switching its value this is done
  instead of shrinking the plot region.

  If \code{dev.print} is used with a specified \code{device} (even
  \code{postscript}) it sets the width and height in the same way as
  \code{dev.copy2eps}.  This will not be appropriate unless the device
  specifies dimensions in inches, in particular not for \code{png},
  \code{jpeg} and (Windows only) \code{bmp}.
}
\note{
  Most devices (including all screen devices) have a display list
  which records all of the graphics operations that occur in the device.
  \code{dev.copy} copies graphics contents by copying the display list
  from one device to another device.  Also, automatic redrawing of
  graphics contents following the resizing of a device depends on the
  contents of the display list.

  After the command \code{dev.control("inhibit")}, graphics operations
  are not recorded in the display list so that \code{dev.copy} and
  \code{dev.print} will not copy anything and the contents of a device
  will not be redrawn automatically if the device is resized.

  The recording of graphics operations is relatively expensive in
  terms of memory so the command \code{dev.control("inhibit")} can be
  useful if memory usage is an issue.
}
\seealso{
  \code{\link{dev.cur}} and other \code{dev.xxx} functions
}
\examples{
\dontrun{
x11()
plot(rnorm(10), main="Plot 1")
dev.copy(device=x11)
mtext("Copy 1", 3)
dev.print(width=6, height=6, horizontal=FALSE) # prints it
dev.off(dev.prev())
dev.off()
}
}
\keyword{device}
