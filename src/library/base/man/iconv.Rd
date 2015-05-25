% File src/library/base/man/iconv.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{iconv}
\alias{iconv}
\alias{iconvlist}
\concept{encoding}
\title{Convert Character Vector between Encodings}
\description{
  This uses system facilities to convert a character vector between
  encodings: the \sQuote{i} stands for \sQuote{internationalization}.
}
\usage{
iconv(x, from = "", to = "", sub = NA, mark = TRUE, toRaw = FALSE)

iconvlist()
}

\arguments{
  \item{x}{A character vector, or an object to be converted to a character
    vector by \code{\link{as.character}}, or a list with \code{NULL} and
    \code{raw} elements as returned by \code{iconv(toRaw = TRUE)}.}
  \item{from}{A character string describing the current encoding.}
  \item{to}{A character string describing the target encoding.}
  \item{sub}{character string.  If not \code{NA} it is used to replace
    any non-convertible bytes in the input.  (This would normally be a
    single character, but can be more.)  If \code{"byte"}, the indication is
    \code{"<xx>"} with the hex code of the byte.}
  \item{mark}{logical, for expert use.  Should encodings be marked?}
  \item{toRaw}{logical.  Should a list of raw vectors be returned rather
    than a character vector?}
}

\details{
  The names of encodings and which ones are available are
  platform-dependent.  All \R platforms support \code{""} (for the
  encoding of the current locale), \code{"latin1"} and \code{"UTF-8"}.
  Generally case is ignored when specifying an encoding.

  On most platforms \code{iconvlist} provides an alphabetical list of
  the supported encodings.  On others, the information is on the man
  page for \code{iconv(5)} or elsewhere in the man pages (but beware
  that the system command \code{iconv} may not support the same set of
  encodings as the C functions \R calls).  Unfortunately, the names are
  rarely supported across all platforms.

  Elements of \code{x} which cannot be converted (perhaps because they
  are invalid or because they cannot be represented in the target
  encoding) will be returned as \code{NA} unless \code{sub} is specified.

  Most versions of \code{iconv} will allow transliteration by appending
  \samp{//TRANSLIT} to the \code{to} encoding: see the examples.

  Encoding \code{"ASCII"} is accepted, and on most systems \code{"C"}
  and \code{"POSIX"} are synonyms for ASCII.

  Any encoding bits (see \code{\link{Encoding}}) on elements of \code{x}
  are ignored: they will always be translated as if from encoding
  \code{from} even if declared otherwise.

  Note that implementations of \code{iconv} typically do not do much
  validity checking and will often mis-convert inputs which are invalid
  in encoding \code{from}.
}

\section{Implementation Details}{
  There are three main implementations of \code{iconv} in use.
  Linux's C runtime \samp{glibc} contains one.  Several platforms
  supply GNU \samp{libiconv}, including OS X, FreeBSD and Cygwin, in
  some cases with additional encodings.  On Windows we use a version of
  Yukihiro Nakadaira's \samp{win_iconv}, which is based on Windows'
  codepages.  (We have added many encoding names for compatibility
  with other systems.)  All three have \code{iconvlist}, ignore case in
  encoding names and support \samp{//TRANSLIT} (but with different
  results, and for \samp{win_iconv} currently a \sQuote{best fit}
  strategy is used except for \code{to = "ASCII"}).

  Most commercial Unixes contain an implemetation of \code{iconv} but
  none we have encountered have supported the encoding names we need:
  the \dQuote{R Installation and Administration Manual} recommends
  installing GNU \samp{libiconv} on Solaris and AIX, for example.

  There are other implementations, e.g\sspace{} NetBSD has used one from the
  Citrus project (which does not support \samp{//TRANSLIT}) and there is
  an older FreeBSD port (\samp{libiconv} is usually used there): it has
  not been reported whether or not these work with \R.

  Note that you cannot rely on invalid inputs being detected, especially
  for \code{to = "ASCII"} where some implementations allow 8-bit
  characters and pass them through unchanged or with transliteration.
  
  Some of the implementations have interesting extra encodings: for
  example GNU \samp{libiconv} allows \code{to = "C99"} to use
  \code{\\uxxx} escapes for non-ASCII characters.
}

\section{Byte Order Marks}{
  most commonly known as \sQuote{BOMs}.

  Encodings using character units which are more than one byte in size
  can be written on a file in either big-endian or little-endian order:
  this applies most commonly to UCS-2, UTF-16 and UTF-32/UCS-4
  encodings.  Some systems will write the Unicode character
  \code{U+FEFF} at the beginning of a file in these encodings and
  perhaps also in UTF-8.  In that usage the character is known as a BOM,
  and should be handled during input (see the \sQuote{Encodings} section
  under \code{\link{connection}}: re-encoded connections have some
  special handling of BOMs).  The rest of this section applies when this
  has not been done so \code{x} starts with a BOM.

  Implementations will generally interpret a BOM for \code{from} given
  as one of \code{"UCS-2"}, \code{"UTF-16"} and
  \code{"UTF-32"}.  Implementations differ in how they treat BOMs in
  \code{x} in other \code{from} encodings: they may be discarded,
  returned as character \code{U+FEFF} or regarded as invalid.
}
  
\value{
  If \code{toRaw = FALSE} (the default), the value is a character vector
  of the same length and the same attributes as \code{x} (after
  conversion to a character vector).

  If \code{mark = TRUE} (the default) the elements of the result have a
  declared encoding if \code{to} is \code{"latin1"} or \code{"UTF-8"},
  or if \code{to = ""} and the current locale's encoding is detected as
  Latin-1 (or its superset CP1252 on Windows) or UTF-8.

  If \code{toRaw = TRUE}, the value is a vector of the same length and
  the same attributes as \code{x} whose elements are either \code{NULL}
  (if conversion fails) or a raw vector.

  For \code{iconvlist()}, a character vector (typically of a few hundred
  elements).
}
\note{
  The only reasonably portable name for the ISO 8859-15 encoding,
  commonly known as \sQuote{Latin 9}, is \code{"latin-9"}: some
  platforms support \code{"latin9"} but GNU \samp{libiconv} does not.
  
  Encoding names \code{"utf8"}, \code{"mac"} and \code{"macroman"} are
  not portable.  \code{"utf8"} is converted to \code{"UTF-8"} for
  \code{from} and \code{to} (as from \R 3.0.3) by \code{iconv}, but not
  for e.g.\sspace{}\code{fileEncoding} arguments.  \code{"macintosh"} is
  the official (and most widely supported) name for \sQuote{Mac Roman}
  (\url{http://en.wikipedia.org/wiki/Mac_OS_Roman}).
}

\seealso{
  \code{\link{localeToCharset}}, \code{\link{file}}.
}
\examples{
## In principle, as not all systems have iconvlist
try(utils::head(iconvlist(), n = 50))

\dontrun{
## convert from Latin-2 to UTF-8: two of the glibc iconv variants.
iconv(x, "ISO_8859-2", "UTF-8")
iconv(x, "LATIN2", "UTF-8")
}

## Both x below are in latin1 and will only display correctly in a
## locale that can represent and display latin1.
x <- "fa\xE7ile"
Encoding(x) <- "latin1"
x
charToRaw(xx <- iconv(x, "latin1", "UTF-8"))
xx

iconv(x, "latin1", "ASCII")          #   NA
iconv(x, "latin1", "ASCII", "?")     # "fa?ile"
iconv(x, "latin1", "ASCII", "")      # "faile"
iconv(x, "latin1", "ASCII", "byte")  # "fa<e7>ile"

## Extracts from old R help files (they are nowadays in UTF-8)
x <- c("Ekstr\xf8m", "J\xf6reskog", "bi\xdfchen Z\xfcrcher")
Encoding(x) <- "latin1"
x
try(iconv(x, "latin1", "ASCII//TRANSLIT"))  # platform-dependent
iconv(x, "latin1", "ASCII", sub = "byte")
## and for Windows' 'Unicode'
str(xx <- iconv(x, "latin1", "UTF-16LE", toRaw = TRUE))
iconv(xx, "UTF-16LE", "UTF-8")
}
\keyword{ character }
\keyword{ utilities }
