\name{postscriptFonts}
\alias{postscriptFont}
\alias{postscriptFonts}
\concept{fonts}
\concept{encoding}
\title{PostScript Fonts}
\description{
  These functions handle the translation of a generic R graphics
  font family name to a PostScript font description.
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
  \item{encoding}{the name of an encoding file. Defaults to
#ifdef unix
"ISOLatin1.enc"
#endif
#ifdef windows
"WinAnsi.enc"
#endif
    in the \file{R\_HOME/afm} directory, which is used
    if the path does not contain a path separator.  An extension
    \code{".enc"} can be omitted.}
  \item{\dots}{either character strings naming mappings to display,
    or new (named) mappings to define.}
}
\details{
  A PostScript device is created with a default font (see
  the documentation for \code{postscript}), but it is also possible
  to specify a font family when drawing to the device
  (for example, see the documentation for \code{gpar} in the
  grid package).

  The font family sent to the device is a simple string name, which must be
  mapped to something more specific to PostScript fonts.  A list of
  mappings is maintained and can be modified by the user.

  The \code{postscriptFonts} function can be used to list existing
  translations and to define new mappings.  The \code{postscriptFont}
  function can be used to create a new mapping.
}
\section{Families}{
  The argument \code{family} specifies the font family to be used.
  Default mappings are provided for four
  device-independent family names: \code{"sans"} for a
  sans-serif font,
  \code{"serif"} for a serif font, \code{"mono"} for a monospaced font,
  and \code{"symbol"} for a symbol font. 

  Mappings for a number of standard Adobe fonts
  (and URW equivalents) are also provided:
   \code{"AvantGarde"},
  \code{"Bookman"}, \code{"Courier"}, \code{"Helvetica"},
  \code{"Helvetica-Narrow"}, \code{"NewCenturySchoolbook"},
  \code{"Palatino"} and \code{"Times"}; 
  \code{"URWGothic"}, \code{"URWBookman"}, \code{"NimbusMon"},
  \code{"NimbusSan"}, \code{"NimbusSanCond"}, \code{"CenturySch"},
  \code{"URWPalladio"} and \code{"NimbusRom"}.  

  It is also possible to specify \code{family="ComputerModern"}. This
  is intended to use with the Type 1 versions of the TeX CM fonts.  It
  will normally be possible to include such output in TeX or LaTeX
  provided it is processed with \code{dvips -Ppfb -j0} or the equivalent
  on your system. (\code{-j0} turns off font subsetting.)
}
\section{Metrics}{
  The \code{metrics} argument should be a
  character vector of four or five paths to Adobe Font Metric files for
  the regular, bold, italic, bold italic and (optionally) symbol fonts to
  be used.  If these paths do not contain the file separator, they are
  taken to refer to files in the \R directory \file{R\_HOME/afm}.  Thus the
  default Helvetica family can be specified by \code{family =
    c("hv______.afm",
    "hvb_____.afm", "hvo_____.afm", "hvbo____.afm", "sy______.afm")}.
  It is the user's responsibility to check that suitable fonts are made
  available, and that they contain the needed characters when
  re-encoded.  The fontnames used are taken from the \code{FontName}
  fields of the afm files.  The software including the PostScript plot
  file should either embed the font outlines (usually from \file{.pfb}
  or \file{.pfa} files) or use DSC comments to instruct the print
  spooler to do so.

  The \code{.afm} files for the first four fonts do not need to be in
  the correct encoding, but that for the symbol font must be.

  When \code{family="ComputerModern"} is used, the italic/bold-italic
  fonts used are slanted fonts (\code{cmsl10} and \code{cmbxsl10}).  To
  use text italic fonts instead, define a mapping with
  \code{metrics = c("CM_regular_10.afm", "CM_boldx_10.afm", "cmti10.afm",
    "cmbxti10.afm", "CM_symbol_10.afm")}.
}

\section{Encodings}{
  Encodings describe which glyphs are used to display the character codes
  (in the range 0--255).  By default, \R uses ISOLatin1 on unix and
  WinAnsi on Windows, and
  the examples for \code{\link{text}} are in that encoding.  However,
  the encoding used on machines running \R may well be different, and by
  using the \code{encoding} argument the glyphs can be matched to
  encoding in use.

  None of this will matter if only ASCII characters (codes 32--126) are
  used as all the encodings agree over that range.  Some encodings are
  supersets of ISOLatin1, too.  However, if accented and special
  characters do not come out as you expect, you may need to change the
  encoding.  Three other encodings are supplied with \R:
  \code{"WinAnsi.enc"} and \code{"MacRoman.enc"} correspond to the
  encodings normally used on Windows and MacOS (at least by Adobe), and
  \code{"PDFDoc.enc"} is the first 256 characters of the Unicode
  encoding, the standard for PDF.

  If you change the encoding, it is your responsibility to ensure that
  the PostScript font contains the glyphs used.  One issue here is the Euro
  symbol which is in the WinAnsi and MacRoman encodings but may well not
  be in the PostScript fonts.  (It is in the URW variants; it is not in
  the supplied Adobe Font Metric files.)

  There is one exception.  Character 45 (\code{"-"}) is always set
  as minus (its value in Adobe ISOLatin1) even though it is hyphen in
  the other encodings.  Hyphen is available as character 173 (octal
  0255) in ISOLatin1.
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
CMitalic <- postscriptFont("ComputerModern",
                           c("CM_regular_10.afm", "CM_boldx_10.afm",
                             "cmti10.afm", "cmbxti10.afm",
                             "CM_symbol_10.afm"))
postscriptFonts(CMitalic=CMitalic)
}
\keyword{device}
