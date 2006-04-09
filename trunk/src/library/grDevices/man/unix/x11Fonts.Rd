\name{X11Fonts}
\alias{X11Font}
\alias{X11Fonts}
\concept{fonts}
\concept{encoding}
\title{X11 Fonts}
\description{
  These functions handle the translation of a device-independent R graphics
  font family name to an X11 font description.
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
  An X11 device is created with a default font (see
  the documentation for \code{X11}), but it is also possible
  to specify a font family when drawing to the device
  (for example, see the documentation for \code{gpar} in the
  grid package).

  The font family sent to the device is a simple string name, which must be
  mapped to something more specific to X11 fonts.  A list of
  mappings is maintained and can be modified by the user.

  The \code{X11Fonts} function can be used to list existing
  mappings and to define new mappings.  The \code{X11Font}
  function can be used to create a new mapping.

  Default mappings are provided for four
  device-independent font family names: \code{"sans"} for a
  sans-serif font,
  \code{"serif"} for a serif font, \code{"mono"} for a monospaced font,
  and \code{"symbol"} for a symbol font.
}
\seealso{
  \code{\link{X11}}
}
\examples{
X11Fonts()
X11Fonts("mono")
utopia <- X11Font("-*-utopia-*-*-*-*-*-*-*-*-*-*-*-*")
X11Fonts(utopia=utopia)
}
\keyword{device}
