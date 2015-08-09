% File src/library/grDevices/man/windows/windowsFonts.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{windowsFonts}
\alias{windowsFont}
\alias{windowsFonts}
\concept{fonts}
\title{Windows Fonts}
\description{
  These functions handle the translation of a device-independent R graphics
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
  to specify a font family when drawing to the device (for example, see
  the documentation for \code{"family"} in \code{\link{par}} and for
  \code{"fontfamily"} in \code{\link{gpar}} in the \pkg{grid} package).

  The font family sent to the device is a simple string name, which must be
  mapped to something more specific to windows fonts.  A list of
  mappings is maintained and can be modified by the user.

  The \code{windowsFonts} function can be used to list existing
  mappings and to define new mappings.  The \code{windowsFont}
  function can be used to create a new mapping.

  Default mappings are provided for three device-independent font family
  names: \code{"sans"} for a sans-serif font, \code{"serif"} for a serif
  font and \code{"mono"} for a monospaced font.

  These mappings will only be used if the current font face is
  1 (plain), 2 (bold), 3 (italic), or 4 (bolditalic).
}
\seealso{
  \code{\link{windows}}
}
\examples{
windowsFonts()
windowsFonts("mono")

\dontrun{## set up for Japanese: needs the fonts installed
windows()  # make sure we have the right device type
Sys.setlocale("LC_ALL", "ja")
windowsFonts(JP1 = windowsFont("MS Mincho"),
             JP2 = windowsFont("MS Gothic"),
             JP3 = windowsFont("Arial Unicode MS"))
plot(1:10)
text(5, 2, "\u{4E10}\u{4E00}\u{4E01}", family = "JP1")
text(7, 2, "\u{4E10}\u{4E00}\u{4E01}", family = "JP1", font = 2)
text(5, 1.5, "\u{4E10}\u{4E00}\u{4E01}", family = "JP2")
text(9, 2, "\u{5100}", family = "JP3")
}}
\keyword{device}
