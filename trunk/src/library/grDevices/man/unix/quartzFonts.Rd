\name{quartzFonts}
\alias{quartzFont}
\alias{quartzFonts}
\concept{fonts}
\concept{encoding}
\title{quartz Fonts}
\description{
  These functions handle the translation of a device-independent R graphics
  font family name to a quartz font description.
}
\usage{
quartzFont(family)

quartzFonts(...)	   
}
\arguments{
  \item{family}{a character vector containing the four PostScript font
    names for plain, bold, italic, and bolditalic versions of
    a font family.}
  \item{\dots}{either character strings naming mappings to display,
    or new (named) mappings to define.}
}
\details{
  A quartz device is created with a default font (see
  the documentation for \code{quartz}), but it is also possible
  to specify a font family when drawing to the device
  (for example, see the documentation for \code{gpar} in the
  grid package).

  The font family sent to the device is a simple string name, which must be
  mapped to something more specific to quartz fonts.  A list of
  mappings is maintained and can be modified by the user.

  The \code{quartzFonts} function can be used to list existing
  mappings and to define new mappings.  The \code{quartzFont}
  function can be used to create a new mapping.

  Default mappings are provided for four
  device-independent font family names: \code{"sans"} for a
  sans-serif font,
  \code{"serif"} for a serif font, \code{"mono"} for a monospaced font,
  and \code{"symbol"} for a symbol font.
}
\seealso{
  \code{\link{quartz}}
}
\examples{
quartzFonts()
quartzFonts("mono")
}
\keyword{device}
