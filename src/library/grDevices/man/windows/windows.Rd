\name{windows}
\alias{windows}
\alias{win.graph}
\alias{win.metafile}
\alias{win.print}
\alias{x11}
\alias{X11}
\alias{print.SavedPlots}
\alias{[.SavedPlots}

\title{Windows graphics devices}

\usage{
windows(width = 7, height = 7, pointsize = 12,
        record = getOption("graphics.record"),
        rescale = c("R", "fit", "fixed"), xpinch, ypinch,
        bg = "transparent", canvas = "white",
        gamma = getOption("gamma"), xpos = NA, ypos = NA,
        buffered = getOption("windowsBuffered"))

win.graph(width = 7, height = 7, pointsize = 12)
x11(width = 7, height = 7, pointsize = 12)
X11(width = 7, height = 7, pointsize = 12)
win.metafile(filename = "", width = 7, height = 7, pointsize = 12)
win.print(width = 7, height = 7, pointsize = 12, printer = "")
}

\arguments{
  \item{filename}{the name of the output file: it will be an enhanced
    Windows metafile, usually given extension \code{.emf} or
    \code{.wmf}. Up to 511 characters are allowed.
    The page number is substituted if an integer format is included
    in the character string. (The result must be less than 600
    characters long.)  The default, \code{""}, means the clipboard.
  }
  \item{width}{the (nominal) width of the plotting window in inches.}
  \item{height}{the (nominal) height of the plotting window in inches.}
  \item{pointsize}{the default pointsize of plotted text. The allowed
    range is \code{[6, 48]}: values outside that range are reset to 12.}
  \item{record}{logical: sets the initial state of the flag for
    recording plots.}
  \item{rescale}{controls the action for resizing plots.}
  \item{xpinch, ypinch}{double.  Pixels per inch, horizontally and
    vertically.}
  \item{bg}{color.  The default background color.}
  \item{canvas}{color.  The color of the canvas which is visible
    when the background color is transparent.}
  \item{gamma}{the gamma correction factor.  This value is used
    to ensure that the colors displayed are linearly related to
    RGB values. By default this is taken from
    \code{\link{options}("gamma"}, or is 1 (no correction) if that is
    unset.  It sets \code{par("gamma")} for the device.}
  \item{xpos, ypos}{integer.  Position of the top left of the window, in
    pixels.  Negative values are taken from the opposite edge of the
    monitor.  Missing values meant take the default from the
    \file{\link{Rconsole}} file, which in turn defaults to
    \code{xpos=-25, ypos=0}: this puts the right edge of the windows 25
    pixels from the right edge of the monitor.}
  \item{buffered}{logical.  Should the screen output be double-buffered?}
  \item{printer}{The name of a printer as known to Windows. The default
    causes a dialog box to come for the user to choose a printer.}
}

\description{
  A graphics device is opened. For \code{windows}, \code{win.graph},
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
  By default a screen device asks Windows for the number of pixels per
  inch.  This can be overridden (it is often wrong) by specifying
  \code{xpinch} and \code{ypinch} or the corresponding options
  \code{"xpinch"} and \code{"ypinch"}.

  A graphics window is not allowed to be specified at more that 85\% of
  the screen width or height: the width and height are rescaled
  proportionally. The window can be resized to a larger size.  In MDI mode,
  the limit is 85\% of the MDI client region.

  If the \code{filename} is omitted for a \code{win.metafile} device, the
  output is copied to the clipboard when the device is closed.

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

  The displayed region may be bigger than the \sQuote{paper} size, and
  areas outside the \sQuote{paper} are coloured in the Windows
  application background colour.  Graphics parameters such as
  \code{"din"} refer to the scaled plot if rescaling is in effect.

  The different colours need to be distinguished carefully. The area
  outside the device region is coloured in the Windows application background
  colour.  The device region is coloured in the canvas colour.  This is
  over-painted by the background colour of a plot when a new page is
  called for, but that background colour can be transparent (and is by
  default).  The difference between setting the canvas colour and the
  background colour is that when the device is copied the background
  colour is copied but the canvas colour is not.

  Recorded plot histories are of class \code{"SavedPlots"}.  They have a
  \code{print} method, and a subset method.  As the individual plots are
  of class \code{"recordedplot"} they can be replayed by printing them:
  see \code{\link{recordPlot}}.  The active plot history is stored in
  variable \code{.SavedPlots} in package \pkg{base}.

  When a screen device is double-buffered (the default from \R 1.8.0) the
  screen is updated 100ms after last plotting call or every 500ms during
  continuous plotting.  These times can be altered by setting
  \code{options("windowsTimeout")} to a vector of two integers before
  opening the device.

  The font used for text drawn in a Windows device may be controlled in
  two ways.  The file \code{RHOME\\etc\\Rdevga} can be used to specify
  mappings for \code{par(font=)} (in the graphics
  package). Alternatively, a device-independent R graphics font family
  can be specified (e.g., via \code{par(family=)} in the graphics
  package) and this will be mapped via the Windows font database (see
  \code{windowsFonts}).

  Line widths as controlled by \code{par(lwd=)} are in multiples of
  1/96inch.  Multiples less than 1 are allowed, down to one pixel width.

  \code{pch="."} with \code{cex = 1} corresponds to a rectangle of sides
  the larger of one pixel and 0.01 inch.
}

\value{
  A plot device is opened: nothing is returned to the \R interpreter.
}

\seealso{
  \code{\link{windowsFonts}},
  \code{\link{savePlot}}, \code{\link{bringToTop}},
  \code{\link{Devices}}, \code{\link{postscript}}
}
\examples{\dontrun{## A series of plots written to a sequence of metafiles
win.metafile("Rplot\%02d.wmf", pointsize = 10)
}}
\keyword{device}
