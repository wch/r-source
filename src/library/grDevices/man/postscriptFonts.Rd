\name{postscriptFonts}
\alias{postscriptFont}
\alias{postscriptFonts}
\concept{fonts}
\concept{encoding}
\title{PostScript Fonts}
\description{
  These functions handle the translation of a device-independent R graphics
  font family name to a PostScript font description, used by both the
  \code{\link{postscript}} and \code{\link{pdf}} graphics devices.
}
\usage{
postscriptFont(family, metrics, encoding = "default")

postscriptFonts(...)	   
}
\arguments{
  \item{family}{a character string giving the name of an Adobe Type 1
    font family.}
  \item{metrics}{a vector of four or five strings giving paths
    to the afm (font metric) files for the Type 1 font.}
  \item{encoding}{the name of an encoding file.  Defaults to
    \code{"default"}, which maps to
#ifdef unix
"ISOLatin1.enc"
#endif
#ifdef windows
"WinAnsi.enc"
#endif
    Otherwise, a file name in the \file{enc} directory of the
    \pkg{grDevices} package, which is used if the path does not contain a
    path separator.  An extension \code{".enc"} can be omitted.}
  \item{\dots}{either character strings naming mappings to display,
    or new (named) mappings to define.}
}
\details{
  A PostScript device is created with a default font (see the
  documentation for \code{\link{postscript}}), but it is also possible
  to specify a font family when drawing to the device (for example, see
  the documentation for \code{\link{gpar}} in the grid package).
  The font mechanism is shared with the \code{\link{pdf}} device.

  The font family sent to the device is a simple string name, which must be
  mapped to something more specific to PostScript fonts.  A list of
  mappings is maintained and can be modified by the user.

  The \code{postscriptFonts} function can be used to list existing
  translations and to define new mappings.  The \code{postscriptFont}
  function can be used to create a new mapping.

  The argument \code{family} specifies the font family to be used.
  Default mappings are provided for four device-independent family
  names: \code{"sans"} for a sans-serif font, \code{"serif"} for a serif
  font, \code{"mono"} for a monospaced font, and \code{"symbol"} for a
  symbol font.

  Mappings for a number of standard Adobe fonts (and URW equivalents)
  are also provided: \code{"AvantGarde"},
  \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
  \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
  \code{"Palatino"} and \code{"Times"};
  \code{"URWGothic"}, \code{"URWBookman"}, \code{"NimbusMon"},
  \code{"NimbusSan"} (synonym \code{"URWHelvetica"}),
  \code{"NimbusSanCond"}, \code{"CenturySch"}, \code{"URWPalladio"} and
  \code{"NimbusRom"} (synonym \code{"URWTimes"}).
  
  There are also mappings for \code{"ComputerModern"} and
  \code{"ComputerModernItalic"}.

  The specification of font metrics and encodings is described in the help
  for the \code{\link{postscript}} function.

  The fonts are not embedded in the resulting PostScript file, so
  software including the PostScript plot
  file should either embed the font outlines (usually from \file{.pfb}
  or \file{.pfa} files) or use DSC comments to instruct the print
  spooler to do so.
}
\value{
  \code{postscriptFont} returns a PostScript font description.

  \code{postscriptFonts} returns one or more font mappings.
}
\seealso{
  \code{\link{postscript}}
}
\author{
  Support for Computer Modern fonts is based on a contribution by
  Brian D'Urso \email{durso@hussle.harvard.edu}.
}
\examples{
postscriptFonts()
## This duplicates "ComputerModernItalic".
CMitalic <- postscriptFont("ComputerModern",
                           c("CM_regular_10.afm", "CM_boldx_10.afm",
                             "cmti10.afm", "cmbxti10.afm",
                             "CM_symbol_10.afm"),
                           encoding = "TeXtext.enc")
postscriptFonts(CMitalic=CMitalic)
}
\keyword{device}
