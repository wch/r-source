% File src/library/grDevices/man/unix/x11.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{x11}
\alias{x11}
\alias{X11}
\alias{X11.options}

\title{X Window System Graphics}
\description{
  \code{X11} starts a graphics device driver for the X Window System
  (version 11).  This can only be done on machines/accounts that have
  access to an X server.

  \code{x11} is recognized as a synonym for \code{X11}.

  The \R function is a wrapper for two devices, one based on Xlib
  (\url{https://en.wikipedia.org/wiki/Xlib}) and one using cairographics
  (\url{http://www.cairographics.org}).
}
\usage{
X11(display = "", width, height, pointsize, gamma, bg, canvas,
    fonts, family, xpos, ypos, title, type, antialias)

X11.options(\dots, reset = FALSE)
}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \env{DISPLAY}.  This is ignored (with a warning) if an X11 device is
    already open on another display.}

  \item{width, height}{the width and height of the plotting window, in
    inches.  If \code{NA}, taken from the resources and if
    not specified there defaults to \code{7} inches.  See also
    \sQuote{Resources}.}

  \item{pointsize}{the default pointsize to be used.  Defaults to \code{12}.}

  \item{gamma}{gamma correction fudge factor.
    Colours in R are sRGB;  if your monitor does not conform to
    sRGB, you might be able to improve things by tweaking this
    parameter to apply additional gamma correction to the RGB channels.
    By default 1 (no additional gamma correction).}

  \item{bg}{colour, the initial background colour.  Default
    \code{"transparent"}.}

  \item{canvas}{colour.  The colour of the canvas, which is visible only
    when the background colour is transparent.  Should be an opaque colour
    (and any alpha value will be ignored).  Default \code{"white"}.}

  \item{fonts}{for \code{type = "Xlib"} only:
    X11 font description strings into which weight, slant and
    size will be substituted.  There are two, the first for fonts 1 to 4
    and the second for font 5, the symbol font.  See section \sQuote{Fonts}.}

  \item{family}{The default family: a length-one character string.  This
    is primarily intended for cairo-based devices, but for \code{type =
    "Xlib"}, the \code{\link{X11Fonts}()} database is used to map family
    names to \code{fonts} (and this argument takes precedence over that
    one).}

  \item{xpos, ypos}{integer: initial position of the top left corner of the
    window, in pixels.  Negative values are from the opposite corner,
    e.g.\sspace{}\code{xpos = -100} says the top right corner should be 100 pixels
    from the right edge of the screen.  If \code{NA} (the default),
    successive devices are cascaded in 20 pixel steps from the top left.
    See also \sQuote{Resources}.}

  \item{title}{character string, up to 100 bytes.  With the default,
    \code{""}, a suitable title is created internally.  A C-style format
    for an integer will be substituted by the device number (see the
    \code{file} argument to \code{\link{postscript}} for further
    details).  How non-ASCII titles are handled is
    implementation-dependent.}

  \item{type}{character string, one of \code{"Xlib"}, \code{"cairo"},
    \code{"nbcairo"} or \code{"dbcairo"}.  Only the first will be
    available if the system was compiled without support for
    cairographics.  The default is \code{"cairo"} where available except
    on macOS, otherwise \code{"Xlib"}.}

  \item{antialias}{for cairo types, the type of anti-aliasing (if any)
    to be used.  One of \code{c("default", "none", "gray", "subpixel")}.}

  \item{reset}{logical: should the defaults be reset to their defaults?}

  \item{\dots}{Any of the arguments to \code{X11}, plus \code{colortype}
    and \code{maxcubesize} (see section \sQuote{Colour Rendering}).}
}
\details{
  The defaults for all of the arguments of \code{X11} are set by
  \code{X11.options}: the \sQuote{Arguments} section gives the
  \sQuote{factory-fresh} defaults.

  The initial size and position are only hints, and may not be acted on
  by the window manager.  Also, some systems (especially laptops) are
  set up to appear to have a screen of a different size to the physical
  screen.

  Option \code{type} selects between two separate devices: \R can be
  built with support for neither, \code{type = "Xlib"} or both.  Where
  both are available, types \code{"cairo"}, \code{"nbcairo"} and
  \code{"dbcairo"} offer
  \itemize{
    \item
    antialiasing of text and lines.
    \item
    translucent colours.
    \item
    scalable text, including to sizes like 4.5 pt.
    \item
    full support for UTF-8, so on systems with suitable fonts you can
    plot in many languages on a single figure (and this will work even
    in non-UTF-8 locales).  The output should be locale-independent.
  }

  There are three variants of the cairo-based device.  \code{type =
  "nbcairo"} has no buffering.  \code{type = "cairo"} has some
  buffering, and supports \code{\link{dev.hold}} and \code{dev.flush}.
  \code{type = "dbcairo"} buffers output and updates the screen about
  every 100ms (by default).  The refresh interval can be set (in units
  of seconds) by e.g.\sspace{}\code{\link{options}(X11updates = 0.25)}: the
  value is consulted when a device is opened.  Updates are only looked
  for every 50ms (at most), and during heavy graphics computations only
  every 500ms.

  Which version will be fastest depends on the X11 connection and the
  type of plotting.  You will probably want to use a buffered type
  unless backing store is in use on the X server (which for example it
  always is on macOS displays), as otherwise repainting when the
  window is exposed will be slow.  On slow connections \code{type =
    "dbcairo"} will probably give the best performance.

  Because of known problems with font selection on macOS without
  Pango (for example, the CRAN distribution), \code{type = "cairo"} is
  not the default there.  These problems have included mixing up bold
  and italic (since worked around), selecting incorrect glyphs and ugly
  or missing symbol glyphs.

  All devices which use an X11 server (including the \code{type =
    "Xlib"} versions of bitmap devices such as \code{\link{png}}) share
  internal structures, which means that they must use the same
  \code{display} and visual.  If you want to change display, first close
  all such devices.

  The cursor shown indicates the state of the device.  If quiescent the
  cursor is an arrow: when the locator is in use it is a crosshair
  cursor, and when plotting computations are in progress (and this can
  be detected) it is a watch cursor.  (The exact cursors displayed will
  depend on the window manager in use.)
}

\section{X11 Fonts}{
  This section applies only to \code{type = "Xlib"}.

  An initial/default font family for the device can be specified via
  the \code{fonts} argument, but if a device-independent R graphics font
  family is specified (e.g., via \code{par(family =)} in the graphics
  package), the X11 device makes use of the X11 font database (see
  \code{X11Fonts}) to convert the R graphics font family to an
  X11-specific font family description.  If \code{family} is supplied as
  an argument, the X11 font database is used to convert that, but
  otherwise the argument \code{fonts} (with default given by
  \code{X11.options}) is used.

  X11 chooses fonts by matching to a pattern, and it is quite possible
  that it will choose a font in the wrong encoding or which does not
  contain glyphs for your language (particularly common in
  \code{iso10646-1} fonts).

  The \code{fonts} argument is a two-element character vector, and the
  first element will be crucial in successfully using
  non-Western-European fonts.  Settings that have proved useful include

  \code{"-*-mincho-\%s-\%s-*-*-\%d-*-*-*-*-*-*-*"} for CJK languages and
  \code{"-cronyx-helvetica-\%s-\%s-*-*-\%d-*-*-*-*-*-*-*"} for Russian.

  For UTF-8 locales, the \code{XLC_LOCALE} databases provide mappings
  between character encodings, and you may need to add an entry for your
  locale (e.g., Fedora Core 3 lacked one for \code{ru_RU.utf8}).
}

\section{Cairo Fonts}{
  The cairographics-based devices work directly with font family names
  such as \code{"Helvetica"} which can be selected initially by the
  \code{family} argument and subsequently by \code{\link{par}} or
  \code{\link{gpar}}.  There are mappings for the three
  device-independent font families, \code{"sans"} for a sans-serif font
  (to \code{"Helvetica"}), \code{"serif"} for a serif font (to
  \code{"Times"}) and \code{"mono"} for a monospaced font (to
  \code{"Courier"}).

  The font selection is handled by \code{Pango} (usually \emph{via}
  \code{fontconfig}) or \code{fontconfig} (on macOS and perhaps
  elsewhere).  The results depend on the fonts installed on the system
  running \R -- setting the environmnent variable \env{FC_DEBUG} to 1
  normally allows some tracing of the selection process.

  This works best when high-quality scalable fonts are installed,
  usually in Type 1 or TrueType formats: see the \dQuote{R Installation
  and Administration Manual} for advice on how to obtain and install
  such fonts.  At present the best rendering (including using kerning)
  will be achieved with TrueType fonts: see
  \url{http://www.freedesktop.org/software/fontconfig/fontconfig-user.html}
  for ways to set up your system to prefer them.  The default family
  (\code{"Helvetica"}) is likely not to use kerning: alternatives which
  should if you have them installed are \code{"Arial"},
  \code{"DejaVu Sans"} and \code{"Liberation Sans"} (and perhaps
  \code{"FreeSans"}).  For those who prefer fonts with serifs, try
  \code{"Times New Roman"}, \code{"DejaVu Serif"} and \code{"Liberation
  Serif"}.  To match LaTeX text, use something like \code{"CM Roman"}.

  %  https://bugs.launchpad.net/ubuntu/+source/fontconfig/+bug/551977
  Problems with incorrect rendering of symbols (e.g., of
  \code{quote(pi)} and \code{expression(10^degree))})
  have been seen on Linux systems which have the Wine
  symbol font installed -- \code{fontconfig} then prefers this and
  misinterprets its encoding.  Adding the following lines
  to \file{~/.fonts.conf} or \file{/etc/fonts/local.conf} may circumvent
  this problem by preferring the URW Type 1 symbol font.
\preformatted{<fontconfig>
<match target="pattern">
  <test name="family"><string>Symbol</string></test>
  <edit name="family" mode="prepend" binding="same">
    <string>Standard Symbols L</string>
  </edit>
</match>
</fontconfig>
}
A test for this is to run at the command line \command{fc-match Symbol}.
If that shows \code{symbol.ttf} that may be the Wine symbol font -- use
\command{locate symbol.ttf} to see if it is found from a directory with
\samp{wine} in the name.
}

\section{Resources}{
  The standard X11 resource \code{geometry} can be used to specify the
  window position and/or size, but will be overridden by values
  specified as arguments or non-\code{NA} defaults set in
  \code{X11.options}.  The class looked for is \code{R_x11}.  Note that
  the resource specifies the width and height in pixels and not in
  inches.  See for example \samp{man X} (or
  \url{https://www.x.org/releases/current/}).
  An example line in \file{~/.Xresources} might be
\preformatted{R_x11*geometry: 900x900-0+0
}
  which specifies a 900 x 900 pixel window at the top right of the screen.
}

\section{Colour Rendering}{
  X11 supports several \sQuote{visual} types, and nowadays almost all
  systems support \sQuote{truecolor} which \code{X11} will use by
  default.  This uses a direct specification of any RGB colour up to the
  depth supported (usually 8 bits per colour).  Other visuals make use
  of a palette to support fewer colours, only grays or even only
  black/white.  The palette is shared between all X11 clients, so it can
  be necessary to limit the number of colours used by \R.

  The default for \code{type = "Xlib"} is to use the best possible colour
  model for the visual of the X11 server: these days this will almost
  always be \sQuote{truecolor}.  This can be overridden by the
  \code{colortype} argument of \code{X11.options}.  \bold{Note:} All
  \code{X11} and \code{type = "Xlib"} \code{\link{bmp}}, \code{jpeg},
  \code{png} and \code{tiff} devices share a \code{colortype} which is
  set when the first device to be opened.  To change the
  \code{colortype} you need to close \emph{all} open such devices, and
  then use \code{X11.options(colortype =)}.

  The colortype types are tried in the order \code{"true"},
  \code{"pseudo"}, \code{"gray"} and \code{"mono"} (black or white
  only).  The values \code{"pseudo"} and \code{"pseudo.cube"} provide
  two colour strategies for a pseudocolor visual.  The first strategy
  provides on-demand colour allocation which produces exact colours
  until the colour resources of the display are exhausted (when plotting
  will fail).  The second allocates (if possible) a standard colour
  cube, and requested colours are approximated by the closest value in
  the cube.

  With \code{colortype} equal to \code{"pseudo.cube"} or \code{"gray"}
  successively smaller palettes are tried until one is completely
  allocated.  If allocation of the smallest attempt fails the device will
  revert to \code{"mono"}.  For \code{"gray"} the search starts at 256
  grays for a display with depth greater than 8, otherwise with half
  the available colours.  For \code{"pseudo.cube"} the maximum cube size
  is set by \code{X11.options(maxcolorsize =)} and defaults to
  256.  With that setting the largest cube tried is 4 levels each for
  RGB, using 64 colours in the palette.

  % A test in 2011 showed that cairo >= 1.6 works on 8-bit visuals,
  % but does not interpret colours correctly.  Done via VNC.
  The cairographics-based devices most likely only work (or work
  correctly) with \sQuote{TrueColor} visuals, although in principle this
  depends on the cairo installation: a warning is given if any other
  visual is encountered.

  \code{type = "Xlib"} supports \sQuote{TrueColor},
  \sQuote{PseudoColor}, \sQuote{GrayScale}, \code{StaticGray} and
  \code{MonoChrome} visuals: \sQuote{StaticColor} and
  \sQuote{DirectColor} visuals are handled only in black/white.
}

\section{Anti-aliasing}{
  Anti-aliasing is only supported for cairographics-based devices, and
  applies to both graphics and fonts.  It is generally preferable for
  lines and text, but can lead to undesirable effects for fills,
  e.g.\sspace{}for \code{\link{image}} plots, and so is never used for fills.

  \code{antialias = "default"} is in principle platform-dependent, but
  seems most often equivalent to \code{antialias = "gray"}.
}

\section{Conventions}{
  This section describes the implementation of the conventions for
  graphics devices set out in the \dQuote{R Internals Manual}.

  \itemize{
    \item The default device size is 7 inches square.
    \item Font sizes are in big points.
    \item The default font family is Helvetica.
    \item Line widths in 1/96 inch, minimum one pixel for \code{type =
      "Xlib"}, 0.01 otherwise.
    \item For \code{type = "Xlib"} circle radii are in pixels with
    minimum one.
    \item Colours are interpreted by the X11 server, which is
    \emph{assumed} to conform to sRGB.
  }
}

\seealso{
  \code{\link{Devices}}, \code{\link{X11Fonts}}, \code{\link{savePlot}}.
}
\examples{\dontrun{
## put something like this is your .Rprofile to customize the defaults
setHook(packageEvent("grDevices", "onLoad"),
        function(...) grDevices::X11.options(width = 8, height = 6, xpos = 0,
                                             pointsize = 10))
}}
\keyword{device}
