\name{windows}
\alias{windows}
\alias{win.graph}
\alias{win.metafile}
\alias{win.print}
\alias{x11}
\alias{X11}

\title{Windows graphics devices}

\usage{
windows(width = 7, height = 7, pointsize = 12)
win.graph(width = 7, height = 7, pointsize = 12)
x11(display = "", width = 7, height = 7, pointsize = 12)
X11(display = "", width = 7, height = 7, pointsize = 12)
win.metafile(filename = "", width = 7, height = 7, pointsize = 12)
win.print(width = 7, height = 7, pointsize = 12)
}

\arguments{
    \item{display}{indicates the purpose of the device.}
    \item{filename}{the name of the output file: it will be an enhanced
	Windows metafile, usually given extension \code{.emf} or
	\code{.wmf}.}
    \item{width}{the (nominal) width of the plotting window in inches.}
    \item{height}{the (nominal) height of the plotting window in inches.}
    \item{pointsize}{the default pointsize of plotted text.}
}
\description{
    A graphics device is opened. For \code{win.graph}, \code{windows},
    \code{x11} and \code{X11} this is a graphics window on the current
    Windows display: the mutliple names are for compatibility with other
    systems. \code{win.metafile} prints to a file and \code{win.print} to
    the Windows print system.
}
\details{
    All these devices are implemented as \code{windows} devices, the
    \code{display} parameter selects which is actually used.
 
    The size of a window is computed from information provided about the
    display: it depends on the system being configured accurately.

    A graphics window is not allowed to be specified at more that 85\% of
    the screen width or height: the width and height are rescaled
    proportionally. The window can be resized to a larger size.

    If the \code{filename} is omitted for a \code{win.metafile} device, the
    output is copied to the clipboard when the device is closed. A
    \code{win.metafile} device can only be used for a single page.
}
\value{
    A plot device is opened: nothing is returned to the \R interpreter.
}
\author{Guido Masarotto}

\seealso{\code{\link{Devices}}, \code{\link{postscript}}}

\keyword{device}
