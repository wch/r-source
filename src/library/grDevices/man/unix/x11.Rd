% File src/library/grDevices/man/unix/x11.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
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
}
\usage{
X11(display = "", width, height, pointsize, gamma, bg, canvas,
    fonts, xpos, ypos, title, type, antialias)

X11.options(\dots, reset = FALSE)
}
\arguments{
  \item{display}{the display on which the graphics window will appear.
    The default is to use the value in the user's environment variable
    \env{DISPLAY}.}
  \item{width, height}{the width and height of the plotting window, in
    inches.  If \code{NA}, taken from the resources and if
    not specified there defaults to \code{7} inches.  See also
    \sQuote{Resources}.}
  \item{pointsize}{the default pointsize to be used.  Defaults to \code{12}.}
  \item{gamma}{the gamma correction factor.  This value is used
    to help ensure that the colors perceived are linearly related to
    RGB values (see \code{\link{hsv}}).  By default 1 (default
    correction).}
  \item{bg}{color.  The default background color.  Default
    code{"transparent"}.}
  \item{canvas}{color.  The color of the canvas, which is visible only
    when the background color is transparent.  Default \code{"white"}.}
  \item{fonts}{X11 font description strings into which weight, slant and
    size will be substituted.  There are two, the first for fonts 1 to 4
    and the second for font 5, the symbol font.  See section \sQuote{Fonts}.}
  \item{xpos, ypos}{integer: initial position of the top left corner of the
    window, in pixels.  Negative values are from the opposite corner,
    e.g. \code{xpos=-100} says the top right corner should be 100 pixels
    from the right edge of the screen.  If \code{NA} (the default),
    successive devices are cascaded in 20 pixel steps from the top left.
    See also \sQuote{Resources}.}
  \item{title}{character string, up to 100 bytes.  With the default,
    \code{""}, a suitable title is created internally.  A C-style format
    for an integer will be substituted by the device number (see the
    \code{file} argument to \code{\link{postscript}} for further
    details).  How non-ASCII titles are handled is
    implementation-dependent.}
  \item{type}{character string, one of \code{"Xlib"} (the only type
    prior to \R 2.7.0) or \code{"Cairo"} or \code{"nbCairo"}.  The
    latter two will only be available if the system was compiled with
    support for cairo.}
  \item{antialias}{for cairo types, the type of anti-aliasing (if any)
    to be used.  One of \code{c("default", "none", "gray", "subpixel")}.}
  \item{reset}{logical: should the defaults be reset to their defaults?}
  \item{\dots}{Any of the arguments to \code{X11}, plus \code{colortype}
    and \code{maxcubesize} (see section \sQuote{Color Rendering}).}
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
  both are available, \code{type = "Cairo"} offers
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
  but can be slower (and will be much slower on a slow X11 connection as
  all the rendering is done on the machine running \R rather than in the
  X server).  \code{type = "nbCairo"} is the same device without
  buffering, and will run faster on a mchine with a slow X11 connection
  (although still not as fast as \code{type = "Xlib"}) -- also,
  \code{\link{savePlot}} is not supported.
}
\section{X11 Fonts}{
  This section applies only to \code{type = "Xlib"}.

  An initial/default font family for the device can be specified via
  the \code{fonts} argument, but if a device-independent R graphics font
  family is specified (e.g., via \code{par(family=)} in the graphics
  package), the X11 device makes use of the X11 font database (see
  \code{X11Fonts}) to convert the R graphics font family to an
  X11-specific font family description.

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
  locale (e.g. Fedora Core 3 lacked one for \code{ru_RU.utf8}).
}

\section{Cairo Fonts}{
  The cairo-based device works directly with font family names such as
  \code{"Helvetica"} which should be selected by \code{\link{par}} or
  \code{\link{gpar}}.  There are mappings for the three
  device-independent font families, \code{"sans"} for a sans-serif font
  (to \code{"Helvetica"}), \code{"serif"} for a serif font (to
  \code{"Times"}) and \code{"mono"} for a monospaced font (to
  \code{"Courier"}).

  The font selection is handled by \code{Pango} (usually) or
  \code{cairo} (on Mac OS X and perhaps elsewhere).  Both make use of
  \code{fontconfig} (\url{http://wwww.fontconfig.org}) to select fonts
  and so the results depend on the fonts installed on the system running
  \R -- setting the environmnent variable \env{FC_DEBUG} to 1 allows
  some tracing of the selection process.

  This works best when high-quality scalable fonts are installed,
  usually in Type 1 or TrueType formats: see the \dQuote{R Installation
  and Administration Manual} for advice on how to obtain and install
  such fonts.
}

\section{Resources}{
  The standard X11 resource \code{geometry} can be used to specify the
  window position and/or size, but will be overridden by values
  specified as arguments or non-\code{NA} defaults set in
  \code{X11.options}.  The class looked for is \code{R_x11}.  Note that
  the resource specifies the width and height in pixels and not in
  inches.  See for example
  \url{http://web.mit.edu/answers/xwindows/xwindows_resources.html} and
  perhaps \samp{man X} (or \url{http://www.xfree86.org/current/X.7.html}).
  An example line in \file{~/.Xresources} might be
\preformatted{R_x11*geometry: 900x900-0+0
}
  which specifies a 900 x 900 pixel window at the top right of the screen.
}

\section{Color Rendering}{
  X11 supports several \sQuote{visual} types, and nowadays almost all
  systems support \sQuote{truecolor} which \code{X11} will use by
  default.  This uses a direct specification of any RGB color up to the
  depth supported (usually 8 bits per color).  Other visuals make use
  of a palette to support fewer colors, only grays or even only
  black/white.  The palette is shared between all X11 clients, so it can
  be necessary to limit the number of colors used by \R.

  Cairo-based devices support only \sQuote{truecolor} visuals.

  The default for \code{type="Xlib"} is to use the best possible color
  model for the visual of the X11 server.  This can be overridden by the
  \code{colortype} argument of \code{X11.options}.  \bold{Note:} All
  \code{X11} and \code{type = "Xlib"} \code{\link{bmp}}, \code{jpeg},
  \code{png} and \code{tiff} devices share a \code{colortype} which is
  set when the first device to be opened.  To change the
  \code{colortype} you need to close \emph{all} open such devices, and
  then use \code{X11.options(colortype=)}.

  The colortype types are tried in the order \code{"true"},
  \code{"pseudo"}, \code{"gray"} and \code{"mono"} (black or white
  only).  The values \code{"pseudo"} and \code{"pseudo.cube"} provide
  color strategies for a pseudocolor visual.  The first strategy
  provides on-demand color allocation which produces exact colors until
  the color resources of the display are exhausted (when plotting will
  fail).  The second allocates (if possible) a standard color cube, and
  requested colors are approximated by the closest value in the cube.

  With \code{colortype} equal to \code{"pseudo.cube"} or \code{"gray"}
  successively smaller palettes are tried until one is completely
  allocated.  If allocation of the smallest attempt fails the device will
  revert to \code{"mono"}.  For \code{"gray"} the search starts at 256
  grays for a display with depth greater than 8, otherwise with half
  the available colors.  For \code{"pseudo.cube"} the maximum cube size
  is set by \code{X11.options(maxcolorsize=)} and defaults to
  256.  With that setting the largest cube tried is 4 levels each for
  RGB, using 64 colors in the palette.
}
\section{Anti-aliasing}{
  Anti-aliasing is only supported for cairo-based devices, and applies to
  graphics and to fonts.  It is generally preferable for lines and text,
  but can lead to undesirable effects for fills, e.g. for
  \code{\link{image}} plots, and so is never used for fills.

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
      "Xlib"}.
    \item For \code{type = "Xlib"} circle radii are in pixels with
    minimum one.
    \item Colours are interpreted by the X11 server, normally in a fair
    approximation to sRGB.
  }
}
\seealso{
  \code{\link{Devices}}, \code{\link{X11Fonts}}, \code{\link{savePlot}}.
}
\keyword{device}
