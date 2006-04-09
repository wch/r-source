\name{postscriptFonts}
\alias{postscriptFonts}
\alias{pdfFonts}
\concept{fonts}
\concept{encoding}
\title{PostScript and PDF Font Families}
\description{
  These functions handle the translation of a \R graphics
  font family name to a PostScript or PDF font description, used by the
  \code{\link{postscript}} or \code{\link{pdf}} graphics devices.
}
\usage{
postscriptFonts(...)
pdfFonts(...)
}
\arguments{
  \item{\dots}{either character strings naming mappings to display,
    or named arguments specifying mappings to add or change.}
}
\details{
  If these functions are called with no argument they list all the
  existing mappings, whereas if they are called with named arguments
  they add (or change) mappings.
  
  A PostScript or PDF device is created with a default font family (see the
  documentation for \code{\link{postscript}}), but it is also possible
  to specify a font family when drawing to the device (for example, see
  the documentation for \code{"family"} in \code{\link{par}} and for
  \code{"fontfamily"} in \code{\link{gpar}} in the grid package).

  The font family sent to the device is a simple string name, which must be
  mapped to a set of PostScript fonts.  Separate lists of mappings for
  \code{postscript} and \code{pdf} devices are  maintained for the
  current \R session and can be added to by the user.

  The \code{postscriptFonts} and \code{pdfFonts} functions can be used
  to list existing mappings and to define new mappings.  The
  \code{\link{Type1Font}} and \code{\link{CIDFont}} functions can be
  used to create new mappings, when the \code{xxxFonts} function is used
  to add them to the database.  See the examples.

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

  Finally, there are some default mappings for East Asian locales:
  \code{"Japan1"}, \code{"Japan1HeiMin"}, \code{"Japan1GothicBBB"},
  and \code{"Japan1Ryumin"} for Japanese;
  \code{"Korea1"} and \code{"Korea1deb"} for Korean;
  \code{"GB1"} (Simplified Chinese) for China and Singapore;
  \code{"CNS1"} (Traditional Chinese) for Hong Kong and Taiwan.
                
  The specification of font metrics and encodings is described in the help
  for the \code{\link{postscript}} function.

  The fonts are not embedded in the resulting PostScript or PDF file, so
  software including the PostScript or PDF plot file should either embed
  the font outlines (usually from \file{.pfb} or \file{.pfa} files) or
  use DSC comments to instruct the print spooler to do so.

  A font family has both an \R-level name, the argument name used when
  \code{postscriptFonts} was called, and an internal name, the
  \code{family} component.  These two names are the same for all the
  pre-defined font families.

  Once a font family is in use it cannot be changed.  \sQuote{In use}
  means that it has been specified \emph{via} a \code{family} or
  \code{fonts} argument to an invocation of the same graphics device
  already in the \R session.  (For these purposes \code{xfig} counts the
  same as \code{postscript} but only uses some of the predefined mappings.)
}
\value{
  A list of one or more font mappings.
}
\seealso{
  \code{\link{postscript}} and \code{\link{pdf}};  \code{\link{Type1Font}}
  and \code{\link{CIDFont}} for specifying new font mappings.
}
\author{
  Support for Computer Modern fonts is based on a contribution by
  Brian D'Urso \email{durso@hussle.harvard.edu}.
}
\examples{
postscriptFonts()
## This duplicates "ComputerModernItalic".
CMitalic <- Type1Font("ComputerModern2",
                      c("CM_regular_10.afm", "CM_boldx_10.afm",
                        "cmti10.afm", "cmbxti10.afm",
                         "CM_symbol_10.afm"),
                      encoding = "TeXtext.enc")
postscriptFonts(CMitalic = CMitalic)

## A CID font for Japanese using a different CMap and
## corresponding cmapEncoding. 
`Jp_UCS-2` <- CIDFont("TestUCS2",
                  c("Adobe-Japan1-UniJIS-UCS2-H.afm",
                    "Adobe-Japan1-UniJIS-UCS2-H.afm",
                    "Adobe-Japan1-UniJIS-UCS2-H.afm",
                    "Adobe-Japan1-UniJIS-UCS2-H.afm"),
                  "UniJIS-UCS2-H", "UCS-2")
pdfFonts(`Jp_UCS-2` = `Jp_UCS-2`)
names(pdfFonts())
}
\keyword{device}
