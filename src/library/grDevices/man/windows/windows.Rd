% File src/library/grDevices/man/windows/windows.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

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

\description{
  A graphics device is opened.  For \code{windows}, \code{win.graph},
  \code{x11} and \code{X11} this is a graphics window on the current
  Windows display: the multiple names are for compatibility with other
  systems.  \code{win.metafile} prints to a file and \code{win.print} to
  the Windows print system.
}

\usage{
windows(width, height, pointsize, record, rescale, xpinch, ypinch,
        bg, canvas, gamma, xpos, ypos, buffered, title,
        restoreConsole)

win.graph(width, height, pointsize)
x11(width, height, pointsize)
X11(width, height, pointsize)

win.metafile(filename = "", width = 7, height = 7, pointsize = 12,
             restoreConsole = TRUE)

win.print(width = 7, height = 7, pointsize = 12, printer = "",
          restoreConsole = TRUE)
}

\arguments{
  \item{width, height}{the (nominal) width and height of the canvas of
    the plotting window in inches.  Default \code{7}.}
  \item{pointsize}{the default pointsize of plotted text. Values are 
    rounded to the nearest integer: values less than or equal to zero
    are reset to \code{12}, the default.}
  \item{record}{logical: sets the initial state of the flag for
    recording plots.  Default \code{FALSE}.}
  \item{rescale}{character, one of \code{c("R", "fit", "fixed")}.
    Controls the action for resizing of the device.  Default
    \code{"R"}.  See \sQuote{Details}.}
  \item{xpinch, ypinch}{double.  Pixels per inch, horizontally and
    vertically.  Default \code{NA_real_}, which means to take the
    value from Windows.}
  \item{bg}{color.  The default background color.  Default
    \code{"transparent"}.}
  \item{canvas}{color.  The color of the canvas which is visible
    when the background color is transparent.  Should be a solid color
    (and any alpha value will be ignored).  Default \code{"white"}.}
  \item{gamma}{the gamma correction factor to assume for the display.
    This value is used to ensure that the colors perceived are linearly
    related to RGB values (see \code{\link{hsv}}).  Default \code{1}.}
  \item{xpos, ypos}{integer.  Position of the top left of the window, in
    pixels.  Negative values are taken from the opposite edge of the
    monitor.  Missing values (the default) mean take the default from the
    \file{\link{Rconsole}} file, which in turn defaults to
    \code{xpos=-25, ypos=0}: this puts the right edge of the window 25
    pixels from the right edge of the monitor.}
  \item{buffered}{logical.  Should the screen output be double-buffered?
    Default \code{TRUE}.}
  \item{title}{character string, up to 100 bytes.  With the default
    \code{""}, a suitable title is created internally.  A C-style format
    for an integer will be substituted by the device number.}
  \item{filename}{the name of the output file: it will be an enhanced
    Windows metafile, usually given extension \code{.emf} or
    \code{.wmf}.  Up to 511 characters are allowed.
    The page number is substituted if an integer format is included
    in the character string. (The result must be less than 600
    characters long.)  The default, \code{""}, means the clipboard.
  }
  \item{printer}{The name of a printer as known to Windows. The default
    causes a dialog box to come for the user to choose a printer.}
  \item{restoreConsole}{logical:  see the \sQuote{Details} below.  Defaults to
    \code{FALSE} for screen devices.}
}

\details{
  All these devices are implemented as versions of the same device.

  All arguments of \code{windows} have defaults set by
  \code{\link{windows.options}}: the defaults given in the argument list
  are the defaults for the defaults.  These defaults also apply to the
  internal values of \code{gamma}, \code{xpinch}, \code{ypinch},
  \code{buffered} and \code{restoreConsole} for \code{win.graph},
  \code{x11} and \code{X11}.

  The size of a window is computed from information provided about the
  display: it depends on the system being configured accurately.
  By default a screen device asks Windows for the number of pixels per
  inch.  This can be overridden (it is often wrong) by specifying
  \code{xpinch} and \code{ypinch}, most conveniently \emph{via}
  \code{\link{windows.options}}.

  If the \code{filename} is omitted (or specified as \code{""} for a
  \code{win.metafile} device, the output is copied to the clipboard
  when the device is closed.

  If a screen device is re-sized, the default behaviour (\code{"R"}) is
  to redraw the plot(s) as if the new size had been specified
  originally.  Using \code{"fit"} will rescale the existing plot(s) to
  fit the new device region, preserving the aspect ratio.  Using
  \code{"fixed"} will leave the plot size unchanged, adding scrollbars
  if part of the plot is obscured.

  A graphics window will never be created at more that 85\% of
  the screen width or height, but can be resized to a larger size.
  For the first two \code{rescale} options the width and height are
  rescaled proportionally if necessary, and if \code{rescale = "fit"}
  the plot(s) are rescaled accordingly.  If \code{rescale = "fixed"}
  the initially displayed portion is selected within these constraints,
  separately for width and height.  In MDI mode,
  the limit is 85\% of the MDI client region.

  Using \code{\link{strwidth}} or \code{\link{strheight}} after a window
  has been rescaled (when using \code{"fit"}) gives dimensions in the
  original units, but only approximately as they are derived from the
  metrics of the rescaled fonts (which are in integer sizes)

  The displayed region may be bigger than the \sQuote{paper} size, and
  area(s) outside the \sQuote{paper} are coloured in the Windows
  application background colour.  Graphics parameters such as
  \code{"din"} refer to the scaled plot if rescaling is in effect.

  The different colours need to be distinguished carefully.  Area(s)
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
  variable \code{.SavedPlots} in the workspace.

  When a screen device is double-buffered (the default) the
  screen is updated 100ms after last plotting call or every 500ms during
  continuous plotting.  These times can be altered by setting
  \code{options("windowsTimeout")} to a vector of two integers before
  opening the device.

  The fonts used for text drawn in a Windows device may be controlled in
  two ways.  The file \code{RHOME\\etc\\Rdevga} can be used to specify
  mappings for \code{par(font=)} (in the graphics package).  Alternatively,
  a device-independent \R graphics font family can be specified (e.g.,
  via \code{par(family=)} in the graphics package) and this will be
  mapped via the Windows font database (see \code{\link{windowsFonts}}).

  Line widths as controlled by \code{par(lwd=)} are in multiples of
  1/96inch.  Multiples less than 1 are allowed, down to one pixel width.

  \code{pch="."} with \code{cex = 1} corresponds to a rectangle of sides
  the larger of one pixel and 0.01 inch.

  For \code{win.metafile} only one plot is allowed per file, and Windows
  seems to disallow reusing the file.  So the \emph{only} way to allow
  multiple plots is to use a parametrized \code{filename} as in the example.
  
  The \code{restoreConsole} argument is a temporary fix for a problem
  in the current implementation of several Windows graphics devices,
  and is likely to be removed in an upcoming release.  If set to
  \code{FALSE}, the console will not receive the focus after the new
  device is opened.  

  There is support for semi-transparent colours of lines, fills and text
  on the \code{windows()} device.
}
\section{Gamma}{
  PC displays map RGB numbers non-linearly to luminosities (and the
  eye/brain system also perceives non-linearly).  For a typical PC
  display, a value of \code{gamma} of around \code{2.2} will result in
  the unit steps in the RGB values are being perceived as roughly equal.
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
