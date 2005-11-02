\name{Type1Font}
\alias{Type1Font}
\alias{CIDFont}
\concept{fonts}
\concept{encoding}
\title{Type 1 Fonts}
\description{
  These functions are used to define
  the translation of a device-independent R graphics
  font family name to a Type 1 or CID font description, used by both the
  \code{\link{postscript}} and \code{\link{pdf}} graphics devices.
}
\usage{
Type1Font(family, metrics, encoding = "default")

CIDFont(family, metrics, cmap, encoding, pdfresource = "")
}
\arguments{
  \item{family}{a character string giving the name of a Type 1
    or CID-keyed font family.}
  \item{metrics}{a vector of four or five strings giving paths
    to the afm (font metric) files for the font.}
  \item{cmap}{the name of a CMap file for a CID-keyed font.}
  \item{encoding}{the name of an encoding file.  Defaults to
    \code{"default"}, which maps on this platform to
#ifdef unix
"ISOLatin1.enc".
#endif
#ifdef windows
"WinAnsi.enc".
#endif
    Otherwise, a file name in the \file{enc} directory of the
    \pkg{grDevices} package, which is used if the path does not contain a
    path separator.  An extension \code{".enc"} can be omitted.}
  \item{pdfresource}{A chunk of PDF code;  only required for using
    a CID-keyed font on PDF; users should not be
    expected to provide this.}
}
\value{
  One or more font mappings.
}
\seealso{
  \code{\link{postscript}}, \code{\link{pdf}},
  \code{\link{postscriptFonts}}, and \code{\link{pdfFonts}}.
}
\examples{
## This duplicates "ComputerModernItalic".
CMitalic <- Type1Font("ComputerModern",
                      c("CM_regular_10.afm", "CM_boldx_10.afm",
                        "cmti10.afm", "cmbxti10.afm",
                        "CM_symbol_10.afm"),
                      encoding = "TeXtext.enc")
}
\keyword{device}
