\name{windowsFonts}
\alias{windowsFont}
\alias{windowsFonts}
\concept{fonts}
\concept{encoding}
\title{Windows Fonts}
\description{
  These functions handle the translation of a generic R graphics
  font family name to a windows font description.
}
\usage{
windowsFont(family)

windowsFonts(...)	   
}
\arguments{
  \item{family}{a character vector containing the font family name
    (\code{"TT"} as the first two characters indicates a TrueType font).
  }
  \item{\dots}{either character strings naming mappings to display,
    or new (named) mappings to define.}
}
\details{
  A windows device is created with a default font (see
  the documentation for \code{windows}), but it is also possible
  to specify a font family when drawing to the device
  (for example, see the documentation for \code{gpar} in the
  grid package).

  The font family sent to the device is a simple string name, which must be
  mapped to something more specific to windows fonts.  A list of
  mappings is maintained and can be modified by the user.

  The \code{windowsFonts} function can be used to list existing
  mappings and to define new mappings.  The \code{windowsFont}
  function can be used to create a new mapping.

  Default mappings are provided for four
  device-independent font family names: \code{"sans"} for a
  sans-serif font,
  \code{"serif"} for a serif font, \code{"mono"} for a monospaced font,
  and \code{"symbol"} for a symbol font.
}
\seealso{
  \code{\link{windows}}
}
\examples{
windowsFonts()
windowsFonts("mono")
}
\keyword{device}
