% File src/library/tools/man/charsets.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{charsets}
\alias{Adobe_glyphs}
\alias{charset_to_Unicode}
\docType{data}
\title{Conversion Tables between Character Sets}
\description{
  \code{charset_to_Unicode} is a matrix of Unicode code points with
  columns for the common 8-bit encodings.

  \code{Adobe_glyphs} is a data frame which gives Adobe glyph names for
  Unicode code points.  It has two character columns, \code{"adobe"} and
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

  \code{Adobe_glyphs} includes all the Adobe glyph names which correspond
  to single Unicode characters.  It is sorted by Unicode code point and
  within a point alphabetically on the glyph (there can be more than one
  name for a Unicode code point).  The data are in the file
  \file{\var{\link{R_HOME}}/share/encodings/Adobe_glyphlist}.
}
\source{
  \url{https://partners.adobe.com/public/developer/en/opentype/glyphlist.txt}
}
\examples{
## find Adobe names for ISOLatin2 chars.
latin2 <- charset_to_Unicode[, "ISOLatin2"]
aUnicode <- as.numeric(paste0("0x", Adobe_glyphs$unicode))
keep <- aUnicode \%in\% latin2
aUnicode <- aUnicode[keep]
aAdobe <- Adobe_glyphs[keep, 1]
## first match
aLatin2 <- aAdobe[match(latin2, aUnicode)]
## all matches
bLatin2 <- lapply(1:256, function(x) aAdobe[aUnicode == latin2[x]])
format(bLatin2, justify = "none")
}
\keyword{datasets}
