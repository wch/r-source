\name{charsets}
\alias{Adobe_glyphs}
\alias{charset_to_Unicode}
\docType{data}
\title{Conversion Tables between Character Sets}
\description{
  \code{charset_to_Unicode} is a matrix of Unicode points with columns
  for the common 8-bit encodings.

  \code{Adobe_glyphs} is a dataframe which gives Adobe glyph names for
  Unicode points. It has two character columns, \code{"adobe"} and
  \code{"unicode"} (a 4-digit hex representation).
}
\usage{
charset_to_Unicode

Adobe_glyphs
}
\details{
  \code{charset_to_Unicode} is an integer matrix of class
  \code{c("\link{noquote}", "\link{hexmode}")} so prints in hexadecimal.
  The mappings are those used by \code{libiconv}: there are differences
  in the way quotes and minus/hyphen are mapped between sources (and the
  postscript encoding files use a different mapping).

  \code{Adobe_glyphs} include all the Adobe glyph names which correspond to
  single Unicode characters.  It is sorted by Unicode point and within a
  point alphabetically on the glyph(there can be more than one name for
  a Unicode point).  The data are in the file
  \file{R\_HOME/share/encodings/Adobe\_glyphlist}.
}
\source{
  \url{http://partners.adobe.com/public/developer/en/opentype/glyphlist.txt}
}
\examples{
## find Adobe names for ISOLatin2 chars.
latin2 <- charset_to_Unicode[, "ISOLatin2"]
aUnicode <- as.numeric(paste("0x", Adobe_glyphs$unicode, sep=""))
keep <- aUnicode \%in\% latin2
aUnicode <- aUnicode[keep]
aAdobe <- Adobe_glyphs[keep, 1]
## first match
aLatin2 <- aAdobe[match(latin2, aUnicode)]
## all matches
bLatin2 <- lapply(1:256, function(x) aAdobe[aUnicode == latin2[x]])
format(bLatin2, justify="none")
}
\keyword{datasets}
