\name{Type1Font}
\alias{Type1Font}
\alias{CIDFont}
\concept{fonts}
\concept{encoding}
\title{Type 1 and CID Fonts}
\description{
  These functions are used to define the translation of a \R graphics
  font family name to a Type 1 or CID font descriptions, used by both
  the \code{\link{postscript}} and \code{\link{pdf}} graphics devices.
}
\usage{
Type1Font(family, metrics, encoding = "default")

CIDFont(family, cmap, cmapEncoding, pdfresource = "")
}
\arguments{
  \item{family}{a character string giving the name to be used internally
    for a Type 1 or CID-keyed font family.  This needs to uniquely
    identify each family, so if you modify a family which is in use
    (see \code{\link{postscriptFonts}}) you need to change the family name.}
  \item{metrics}{a character vector of four or five strings giving paths
    to the afm (Adobe Font Metric) files for the font.}
  \item{cmap}{the name of a CMap file for a CID-keyed font.}
  \item{encoding}{for \code{Type1Font}, the name of an encoding file.
    Defaults to \code{"default"}, which maps on this platform to
#ifdef unix
"ISOLatin1.enc".
#endif
#ifdef windows
"WinAnsi.enc".
#endif
    Otherwise, a file name in the \file{enc} directory of the
    \pkg{grDevices} package, which is used if the path does not contain a
    path separator.  An extension \code{".enc"} can be omitted.
  }
  \item{cmapEncoding}{
    The name of a charset encoding to be used with
    the named CMap file: strings will be translated to this encoding
    when written to the file.
  }
  \item{pdfresource}{A chunk of PDF code;  only required for using
    a CID-keyed font on \code{pdf}; users should not be expected to
    provide this.}
}
\details{
  For \code{Type1Fonts}, if four \code{.afm} files are supplied the
  fifth is taken to be \code{"Symbol.afm"}.  Relative paths are taken
  relative to the directory \file{R\_HOME/library/grDevices/afm}.
  The fifth (symbol) font must be in \code{AdobeSym} encoding.
  However, the glyphs in the first four fonts are referenced by name
  and any encoding given within the \code{.afm} files is not used.

  Glyphs in CID-keyed fonts are accessed by ID (number) and not by name.
  The CMap file maps encoded strings (usually in a MBCS) to IDs, so
  \code{cmap} and \code{cmapEncoding} specifications must match.  There
  are no real bold or italic versions of CID fonts (bold/italic were
  very rarely used in traditional CJK topography), and for the
  \code{\link{pdf}} device all four font faces will be identical.
  However, for the \code{\link{postscript}} device, bold and italic (and
  bold italic) are emulated.

  CID-keyed fonts are intended only for use for the glyphs of CJK
  languages, which are all monospaced and are all treated as filling the
  same bounding box.  (Thus \code{\link{plotmath}} will work with such
  characters, but the spacing will be less carefully controlled than
  with Western glyphs.)  The CID-keyed fonts do contain other
  characters, including a Latin alphabet:  non-CJK glyphs are regarded
  as monospaced with half the width of CJK glyphs.  This is often the
  case, but sometimes Latin glyphs designed for proportional spacing are
  used (and may look odd).  We strongly recommend that CID-keyed fonts
  are \bold{only} used for CJK glyphs.
}
\value{
  A list of class \code{"Type1Font"} or \code{"CIDFont"}.
}
\seealso{
  \code{\link{postscript}}, \code{\link{pdf}},
  \code{\link{postscriptFonts}}, and \code{\link{pdfFonts}}.
}
\examples{
## This duplicates "ComputerModernItalic".
CMitalic <- Type1Font("ComputerModern2",
                      c("CM_regular_10.afm", "CM_boldx_10.afm",
                        "cmti10.afm", "cmbxti10.afm",
                        "CM_symbol_10.afm"),
                      encoding = "TeXtext.enc")

\dontrun{
## This could be used by
postscript(family = CMitalic)
## or
postscriptFonts(CMitalic = CMitalic)  # once in a session
postscript(family = "CMitalic", encoding = "TeXtext.enc")
}}
\keyword{device}
