% File src/library/grDevices/man/x11Fonts.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{X11Fonts}
\alias{X11Font}
\alias{X11Fonts}
\concept{fonts}
\title{X11 Fonts}
\description{
  These functions handle the translation of a device-independent R graphics
  font family name to an X11 font description on Unix-alike platforms.
}
\usage{
X11Font(font)

X11Fonts(...)
}
\arguments{
  \item{font}{a character string containing an X11 font description.}
  \item{\dots}{either character strings naming mappings to display,
    or new (named) mappings to define.}
}
\details{
  These functions apply only to an \code{\link{X11}} device with
  \code{type = "Xlib"} -- \code{X11(type = "cairo")} uses a different
  mechanism to select fonts.

  Such a  device is created with a default font (see
  the documentation for \code{\link{X11}}), but it is also possible
  to specify a font family when drawing to the device (for example, see
  the documentation for \code{"family"} in \code{\link{par}} and for
  \code{"fontfamily"} in \code{\link{gpar}} in the \pkg{grid} package).

  The font family sent to the device is a simple string name, which must be
  mapped to something more specific to X11 fonts.  A list of
  mappings is maintained and can be modified by the user.

  The \code{X11Fonts} function can be used to list existing
  mappings and to define new mappings.  The \code{X11Font}
  function can be used to create a new mapping.

  Default mappings are provided for three device-independent font family
  names: \code{"sans"} for a sans-serif font, \code{"serif"} for a serif
  font and \code{"mono"} for a monospaced font.  Further mappings are
  provided for \code{"Helvetica"} (the device default), \code{"Times"},
  \code{"CyrHelvetica"}, \code{"CyrTimes"} (versions of these fonts with
  Cyrillic support, at least on Linux), \code{"Arial"} (on some
  platforms including macOS and Solaris) and \code{"Mincho"} (a CJK
  font).
}
\seealso{
  \code{\link{X11}}
}
\note{
  Available only when \code{\link{capabilities}()[["X11"]]} is true.
}
\examples{
if(capabilities()[["X11"]]) withAutoprint({
X11Fonts()
X11Fonts("mono")
utopia <- X11Font("-*-utopia-*-*-*-*-*-*-*-*-*-*-*-*")
X11Fonts(utopia = utopia)
})
}
\keyword{device}
