\name{windows}
\alias{windows}
\alias{win.graph}
\alias{win.metafile}
\alias{win.print}
\alias{x11}
\alias{X11}

\title{Windows graphics devices}

\usage{
windows(width = 7, height = 7, pointsize = 12,
        record = getOption("graphics.record"),
        rescale = c("R", "fit", "fixed"), xpinch, ypinch)
win.graph(width = 7, height = 7, pointsize = 12)
x11(width = 7, height = 7, pointsize = 12)
X11(width = 7, height = 7, pointsize = 12)
win.metafile(filename = "", width = 7, height = 7, pointsize = 12)
win.print(width = 7, height = 7, pointsize = 12)

}

\arguments{
  \item{display}{indicates the purpose of the device.}
  \item{filename}{the name of the output file: it will be an enhanced
    Windows metafile, usually given extension \code{.emf} or \code{.wmf}.}
  \item{width}{the (nominal) width of the plotting window in inches.}
  \item{height}{the (nominal) height of the plotting window in inches.}
  \item{pointsize}{the default pointsize of plotted text.}
  \item{record}{logical: sets the initial state of the flag for
    recording plots.}
  \item{rescale}{controls the action for resizing plots.}
  \item{xpinch, ypinch}{double.  Pixels per inch, horizontally and vertically.}
}

\description{
  A graphics device is opened. For \code{win.graph}, \code{windows},
  \code{x11} and \code{X11} this is a graphics window on the current
  Windows display: the multiple names are for compatibility with other
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

  By default a screen device asks Windows for the number of pixels per
  inch.  This can be overridden (it is often wrong) by specifying
  \code{xpinch} and \code{ypinch} or the corresponding options
  \code{"xpinch"} and \code{"ypinch"}.

  If a screen device is re-sized, the default behaviour is to redraw the
  plot(s) as if the new size had been specified originally.  Using
  \code{"fit"} will rescale the existing plot(s) to fit the new device
  region, preserving the aspect ratio.  Using \code{"fixed"} will leave
  the plot size unchanged, adding scrollbars if part of the
  plot is obscured.

  A graphics window will never be created at more that 85\% of
  the screen width or height, but can be resized to a larger size.
  For the first two \code{rescale} options the width and height are
  rescaled proportionally if necessary, and if \code{rescale = "fit"}
  the plot(s) are rescaled accordingly.  If \code{rescale = "fixed"}
  the initially displayed portion is selected within these constraints,
  separately for width and height.

  Using \code{\link{strwidth}} or \code{\link{strheight}} after a window
  has been rescaled (when using \code{"fit"}) gives dimensions in the
  original units, but only approximately as they are derived from the
  metrics of the rescaled fonts (which are in integer sizes)

  The displayed region may be bigger than the `paper' size, and areas
  outside the `paper' are coloured light grey.  Graphics parameters such
  as \code{"din"} refer to the scaled plot if rescaling is in effect.
}

\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}

\author{Guido Masarotto}

\seealso{
  \code{\link{bringToTop}}, \code{\link{Devices}}, \code{\link{postscript}}
}

\keyword{device}
