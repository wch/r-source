\name{postscriptFonts}
\alias{postscriptFonts}
\alias{pdfFonts}
\concept{fonts}
\concept{encoding}
\title{PostScript Fonts}
\description{
  These functions handle the translation of a device-independent R graphics
  font family name to a PostScript or PDF font description, used by the
  \code{\link{postscript}} or \code{\link{pdf}} graphics devices.
}
\usage{
postscriptFonts(...)
pdfFonts(...)
}
\arguments{
  \item{\dots}{either character strings naming mappings to display,
    or new (named) mappings to define.}
}
\details{
  A PostScript or PDF device is created with a default font (see the
  documentation for \code{\link{postscript}}), but it is also possible
  to specify a font family when drawing to the device (for example, see
  the documentation for \code{\link{gpar}} in the grid package).

  The font family sent to the device is a simple string name, which must be
  mapped to something more specific to PostScript or PDF fonts.  A list of
  mappings is maintained and can be modified by the user.

  The \code{postscriptFonts} and \code{pdfFonts} functions
  can be used to list existing
  translations and to define new mappings.  The \code{\link{Type1Font}}
  and \code{\link{CIDFont}} 
  functions can be used to create new mappings.

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
  and \code{"Japan1Ryumin"} for the Japan;
  \code{"Korea1"} and \code{"Korea1deb"} for Korea;
  \code{"CNS1"} for China and Singapore;
  \code{"GB1"} for Hong Kong and Taiwan.
                
  The specification of font metrics and encodings is described in the help
  for the \code{\link{postscript}} function.

  The fonts are not embedded in the resulting PostScript or PDF file, so
  software including the PostScript or PDF plot
  file should either embed the font outlines (usually from \file{.pfb}
  or \file{.pfa} files) or use DSC comments to instruct the print
  spooler to do so.
}
\value{
  One or more font mappings.
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
CMitalic <- Type1Font("ComputerModern",
                      c("CM_regular_10.afm", "CM_boldx_10.afm",
                        "cmti10.afm", "cmbxti10.afm",
                         "CM_symbol_10.afm"),
                      encoding = "TeXtext.enc")
postscriptFonts(CMitalic=CMitalic)
}
\keyword{device}
