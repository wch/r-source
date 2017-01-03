% File src/library/grDevices/man/postscriptFonts.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

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
postscriptFonts(\dots)
pdfFonts(\dots)
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
  \code{"fontfamily"} in \code{\link{gpar}} in the \pkg{grid} package).

  The font family sent to the device is a simple string name, which must be
  mapped to a set of PostScript fonts.  Separate lists of mappings for
  \code{postscript} and \code{pdf} devices are  maintained for the
  current \R session and can be added to by the user.

  The \code{postscriptFonts} and \code{pdfFonts} functions can be used
  to list existing mappings and to define new mappings.  The
  \code{\link{Type1Font}} and \code{\link{CIDFont}} functions can be
  used to create new mappings, when the \code{xxxFonts} function is used
  to add them to the database.  See the examples.

  Default mappings are provided for three device-independent family
  names: \code{"sans"} for a sans-serif font (to \code{"Helvetica"}),
  \code{"serif"} for a serif font (to \code{"Times"}) and \code{"mono"}
  for a monospaced font (to \code{"Courier"}).

  Mappings for a number of standard Adobe fonts (and URW equivalents)
  are also provided: \code{"AvantGarde"},
  \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
  \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
  \code{"Palatino"} and \code{"Times"};
  \code{"URWGothic"}, \code{"URWBookman"}, \code{"NimbusMon"},
  \code{"NimbusSan"} (synonym \code{"URWHelvetica"}),
  \code{"NimbusSanCond"}, \code{"CenturySch"}, \code{"URWPalladio"} and
  \code{"NimbusRom"} (synonym \code{"URWTimes"}).

  There are also mappings for \code{"ComputerModern"},
  \code{"ComputerModernItalic"} and \code{"ArialMT"} (Monotype Arial).

  Finally, there are some default mappings for East Asian locales
  described in a separate section.

  The specification of font metrics and encodings is described in the help
  for the \code{\link{postscript}} function.

  The fonts are not embedded in the resulting PostScript or PDF file, so
  software including the PostScript or PDF plot file should either embed
  the font outlines (usually from \file{.pfb} or \file{.pfa} files) or
  use DSC comments to instruct the print spooler or including
  application to do so (see also \code{\link{embedFonts}}).

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
\section{East Asian fonts}{
  There are some default mappings for East Asian locales:\cr
  \code{"Japan1"}, \code{"Japan1HeiMin"}, \code{"Japan1GothicBBB"},
  and \code{"Japan1Ryumin"} for Japanese;
  \code{"Korea1"} and \code{"Korea1deb"} for Korean;
  \code{"GB1"} (Simplified Chinese) for mainland China and Singapore;
  \code{"CNS1"} (Traditional Chinese) for Hong Kong and Taiwan.

  These refer to the following fonts
  \tabular{ll}{
    Japan1 (PS) \tab \code{HeiseiKakuGo-W5}\cr \tab Linotype Japanese printer font\cr
    Japan1 (PDF) \tab \code{KozMinPro-Regular-Acro}\cr \tab from Adobe Reader 7.0 Japanese Font Pack\cr
    Japan1HeiMin (PS) \tab \code{HeiseiMin-W3}\cr \tab Linotype Japanese printer font\cr
    Japan1HeiMin (PDF) \tab \code{HeiseiMin-W3-Acro}\cr \tab from Adobe Reader 7.0 Japanese Font Pack\cr
    Japan1GothicBBB \tab \code{GothicBBB-Medium}\cr \tab Japanese-market PostScript printer font\cr
    Japan1Ryumin \tab \code{Ryumin-Light}\cr \tab Japanese-market PostScript printer font\cr
    Korea1 (PS) \tab \code{Baekmuk-Batang}\cr  \tab TrueType font found on some Linux systems\cr
    Korea1 (PDF) \tab \code{HYSMyeongJoStd-Medium-Acro}\cr  \tab from Adobe Reader 7.0 Korean Font Pack\cr
    Korea1deb (PS) \tab \code{Batang-Regular}\cr  \tab another name for Baekmuk-Batang\cr
    Korea1deb (PDF) \tab \code{HYGothic-Medium-Acro}\cr  \tab from Adobe Reader 4.0 Korean Font Pack\cr
    GB1 (PS) \tab \code{BousungEG-Light-GB}\cr  \tab TrueType font found on some Linux systems\cr
    GB1 (PDF) \tab \code{STSong-Light-Acro}\cr  \tab from Adobe Reader 7.0 Simplified Chinese Font Pack\cr
    CNS1 (PS) \tab \code{MOESung-Regular}\cr \tab Ken Lunde's CJKV resources\cr
    CNS1 (PDF) \tab \code{MSungStd-Light-Acro}\cr  \tab from Adobe Reader 7.0 Traditional Chinese Font Pack\cr
  }
%  \code{Baekmuk-Batang} Can be found at \url{ftp://ftp.mizi.com/pub/baekmuk/}.
  \code{BousungEG-Light-GB} can be found at
  \url{ftp://ftp.gnu.org/pub/non-gnu/chinese-fonts-truetype/}.
  Ken Lunde's CJKV resources are at
  \url{ftp://ftp.oreilly.com/pub/examples/nutshell/cjkv/adobe/samples/}.
  These will need to be installed or otherwise made available to the
  postscript/PDF interpreter such as ghostscript (and not all
  interpreters can handle TrueType fonts).

  You may well find that your postscript/PDF interpreters has been set
  up to provide aliases for many of these fonts.  For example,
  ghostscript on Windows can optionally be installed to map common East
  Asian fonts names to Windows TrueType fonts.  (You may want to add the
  \code{-Acro} versions as well.)

  Adding a mapping for a CID-keyed font is for gurus only.
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
  Brian D'Urso.% formerly \email{durso@hussle.harvard.edu}.
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
