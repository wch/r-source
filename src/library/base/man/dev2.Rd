\name{dev2}
\alias{dev.copy}
\alias{dev.print}
\alias{dev.control}
\title{Copy Graphics Between Multiple Devices}
\usage{
dev.copy(device, \dots, which=dev.next())
dev.print(device=postscript, \dots)
dev.control(displaylist)
}
\arguments{
    \item{device}{A device function (e.g., \code{x11},
        \code{postscript}, \ldots)}
    \item{\dots}{Arguments to the \code{device} function above.
    For \code{dev.print}, this includes \code{which} and by default any
    \code{\link{postscript}} arguments.}
    \item{which}{A device number specifying the device to copy to}
    \item{displaylist}{A character string}
}
\description{
    \code{dev.copy} copies the graphics contents of the current device
    to the device specified by \code{which} or to a new device which has
    been created by the function specified by \code{device} (it is an
    error to specify both \code{which} and \code{device}).

    \code{dev.print} copies the graphics contents of the current device
    to a new device which has been created by the function specified by
    \code{device} and then shuts the new device.  This is most useful
    for producing a postscript copy from an on-screen device.

    \code{dev.control} allows the user to control the recording of
    graphics operations in a device.  If \code{displaylist} is "inhibit"
    then recording is turned off.
}
\value{
    \code{dev.copy} returns the name and number of the device which has
    been copied to.

    \code{dev.print} returns the name and number of the device which has
    been copied from.
}
\note{
    Every device has a display list which records all of the graphics
    operations that occur in the device.  \code{dev.copy} and
    \code{dev.print} copy graphics contents by copying the display list
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
dev.print(width=6, height=6, horizontal=FALSE)
dev.off(dev.prev())
dev.off()
}
}
\keyword{device}
