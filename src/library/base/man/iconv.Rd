% File src/library/base/man/iconv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
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
  \item{x}{a character vector, or an object to be converted to a character
    vector by \code{\link{as.character}}, or a list with \code{NULL} and
    \code{raw} elements as returned by \code{iconv(toRaw = TRUE)}.}
  \item{from}{a character string describing the current encoding.}
  \item{to}{a character string describing the target encoding.}
  \item{sub}{character string.  If not \code{NA} it is used to replace
    any non-convertible bytes in the input.  (This would normally be a
    single character, but can be more.)  If \code{"byte"}, the indication is
    \code{"<xx>"} with the hex code of the byte.
    %% as from R 4.4.0 always zero-pads to 4 hex digits.
    If \code{"Unicode"} and converting from UTF-8, the Unicode point in
    the form \code{"<U+xxxx>"}, or if \code{c99}, a C99-style escape
    \code{"\\uxxxx"}. (For points in a \sQuote{supplementary plane},
    %% zero-padding to eight hex digits is as prescribed by C99.
    \code{"\Uxxxxxxxx"} is used, with zero-padding)}
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
  the supported encodings (including aliases).  On \samp{musl} (as used
  by Alpine Linux and other lightweight Linux distributions) the listing
  is incomplete.  On others, the information is on the man
  page for \code{iconv(5)} or elsewhere in the man pages (but beware
  that the system command \code{iconv} may not support the same set of
  encodings as the C functions \R calls).  Unfortunately, the names are
  rarely supported across all platforms.

  Elements of \code{x} which cannot be converted (perhaps because they
  are invalid or because they cannot be represented in the target
  encoding) will be returned as \code{NA} (or \code{NULL} for
  \code{toRaw = TRUE}) unless \code{sub} is specified.

  Most versions of \code{iconv} will allow transliteration by appending
  \samp{//TRANSLIT} to the \code{to} encoding: see the examples.

  Encoding \code{"ASCII"} is accepted, and on most systems \code{"C"}
  and \code{"POSIX"} are synonyms for ASCII.  Where
  \code{"ASCII//TRANSLIT"} is unsupported by the OS, \code{"ASCII"} is
  used with \code{sub = "c99"} if from UTF-8, else \code{sub =
  "?"}. (However, \I{musl}'s version of \code{"ASCII"} substitutes
  \code{*}.)

  Elements of \code{x} with a declared encoding (UTF-8 or latin1, see
  \code{\link{Encoding}}) are converted from that encoding if \code{from
  = ""}, otherwise they are taken as being in the encoding specified by
  \code{from}.

  Note that implementations of \code{iconv} typically do not do much
  validity checking and will often mis-convert inputs which are invalid
  in encoding \code{from}.

  If \code{sub = "Unicode"} or \code{sub = "c99"} is used for a
  non-UTF-8 input it is the same as \code{sub = "byte"}.
}

\section{Implementation Details}{
  There are three main implementations of \code{iconv} in use.  Linux's
  most common C runtime, \samp{glibc}, contains one.  Several platforms
  supply versions or emulations of GNU \samp{libiconv}, including
  previous versions of macOS and FreeBSD, in some cases with additional
  encodings.  On Windows we use a version of \I{Yukihiro Nakadaira}'s
  \samp{win_iconv}, which is based on Windows' codepages.  (We have
  added many encoding names for compatibility with other systems.)  All
  three have \code{iconvlist}, ignore case in encoding names and support
  \samp{//TRANSLIT} (but with different results, and for
  \samp{win_iconv} currently a \sQuote{best fit} strategy is used except
  for \code{to = "ASCII"}).

%% https://github.com/apple-oss-distributions/libiconv/blob/libiconv-80.1.1/citrus/iconv.h  
  The macOS 14 implementation is attributed to the \sQuote{Citrus
  Project}: the Apple headers declare it as \sQuote{compatible} with GNU
  \samp{libiconv} 1.11 from 2006.  However, it differs in significant
  ways including using transliteration for conversions which cannot be
  represented exactly in the target encoding.  (It seems this
  implementation is also used in recent versions of FreeBSD.  Earlier
  versions of macOS used GNU \samp{libiconv} 1.11 and some
  \acronym{CRAN} builds still do.)  For a failing
  conversion macOS 14 generally translated character(s) to \code{?} but
  14.1 gives an error (so an \code{NA} result in \R).

  Most commercial Unixes contain an implementation of \code{iconv} but
  none we have encountered have supported the encoding names we need:
  the \sQuote{R Installation and Administration} manual recommended
  installing GNU \samp{libiconv} on Solaris and \I{AIX}.

  %% https://wiki.musl-libc.org/functional-differences-from-glibc.html
  %% https://wiki.musl-libc.org/projects-using-musl.html
  Some Linux distributions use \samp{musl} as their C runtime.  This is
  less comprehensive than \samp{glibc}: it does not support
  \samp{//TRANSLIT} but does inexact conversions (currently using
  \samp{*}).

  There are other implementations, e.g.\sspace{}\I{NetBSD} has used one from the
  Citrus project (which does not support \samp{//TRANSLIT}) and there is
  an older FreeBSD port.

  Note that you cannot rely on invalid inputs being detected, especially
  for \code{to = "ASCII"} where some implementations allow 8-bit
  characters and pass them through unchanged or with transliteration or
  substitution.

  Some of the implementations have interesting extra encodings: for
  example GNU \samp{libiconv} and macOS 14 allow \code{to = "C99"} to use
  \samp{\\uxxxx} escapes (or if needed \samp{\Uuxxxxxxxx}) for
  non-ASCII characters.
}

\section{Byte Order Marks}{
  most commonly known as \sQuote{\abbr{BOM}s}.

  Encodings using character units which are more than one byte in size
  can be written on a file in either big-endian or little-endian order:
  this applies most commonly to \I{UCS-2}, UTF-16 and UTF-32/\I{UCS-4}
  encodings.  Some systems will write the Unicode character
  \code{U+FEFF} at the beginning of a file in these encodings and
  perhaps also in UTF-8.  In that usage the character is known as a \abbr{BOM},
  and should be handled during input (see the \sQuote{Encodings} section
  under \code{\link{connection}}: re-encoded connections have some
  special handling of \abbr{BOM}s).  The rest of this section applies when this
  has not been done so \code{x} starts with a \abbr{BOM}.

  Implementations will generally interpret a \abbr{BOM} for \code{from} given
  as one of \code{"UCS-2"}, \code{"UTF-16"} and
  \code{"UTF-32"}.  Implementations differ in how they treat \abbr{BOM}s in
  \code{x} in other \code{from} encodings: they may be discarded,
  returned as character \code{U+FEFF} or regarded as invalid.
}

\value{
  If \code{toRaw = FALSE} (the default), the value is a character vector
  of the same length and the same attributes as \code{x} (after
  conversion to a character vector).  If conversion fails for an element
  that element of the result is set to \code{NA_character_}.  (NB:
  whether conversion fails is implementation-specific.)
  \code{NA_character_} inputs give  \code{NA_character_} outputs.

  If \code{mark = TRUE} (the default) the elements of the result have a
  declared encoding if \code{to} is \code{"latin1"} or \code{"UTF-8"},
  or if \code{to = ""} and the current locale's encoding is detected as
  Latin-1 (or its superset CP1252 on Windows) or UTF-8.

  If \code{toRaw = TRUE}, the value is a list of the same length and
  the same attributes as \code{x} whose elements are either \code{NULL}
  (if conversion fails or the input was \code{NA_character_}) or a raw
  vector.

  For \code{iconvlist()}, a character vector (typically of a few hundred
  elements) of known encoding names.
}
\note{
  The most portable name for the ISO 8859-15 encoding, commonly known as
  \sQuote{Latin 9}, is \code{"iso885915"}: most platforms support both
  \code{"latin-9"} and \code{"latin9"} % (including \samp{musl})
  but GNU \samp{libiconv} does not support the latter.

  Encoding names \code{"utf8"}, \code{"mac"} and \code{"macroman"} are
  not portable.  \code{"utf8"} is converted to \code{"UTF-8"} for
  \code{from} and \code{to} by \code{iconv}, but not
  for e.g.\sspace{}\code{fileEncoding} arguments.  \code{"macintosh"} is
  the official (and most widely supported) name for \sQuote{Mac Roman}
  (\url{https://en.wikipedia.org/wiki/Mac_OS_Roman}).

  Using \code{sub} substitutes each non-convertible \emph{byte} in the
  input, so when converting from UTF-8 a non-convertible character may
  be replaced by two or more bytes.  Using \code{sub = "c99"} or
  \code{sub = "Unicode"} will be clearer.
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
x <- "fran\xE7ais"
Encoding(x) <- "latin1"
x
charToRaw(xx <- iconv(x, "latin1", "UTF-8"))
xx

## The results in the comments are those from glibc and GNU libiconv
iconv(x, "latin1", "ASCII")           #   NA
iconv(x, "latin1", "ASCII", "?")      # "fran?ais"
iconv(x, "latin1", "ASCII", "")       # "franais"
iconv(x, "latin1", "ASCII", "byte")   # "fran<e7>ais"
iconv(xx, "UTF-8", "ASCII", "Unicode")# "fran<U+00E7>ais"
iconv(xx, "UTF-8", "ASCII", "c99")    # "fran\\\\u00e7ais"

## Extracts from old R help files (they are nowadays in UTF-8)
x <- c("Ekstr\xf8m", "J\xf6reskog", "bi\xdfchen Z\xfcrcher")
Encoding(x) <- "latin1"
x
try(iconv(x, "latin1", "ASCII//TRANSLIT"))  # platform-dependent
## glibc gives "Ekstroem" "Joreskog" "bisschen Zurcher"
## macOS 14 gives "Ekstrom" "J\"oreskog" "bisschen Z\"urcher"
## musl gives "Ekstr*m" "J*reskog" "bi*chen Z*rcher"
iconv(x, "latin1", "ASCII", sub = "byte")

## and for Windows' 'Unicode'
str(xx <- iconv(x, "latin1", "UTF-16LE", toRaw = TRUE))
iconv(xx, "UTF-16LE", "UTF-8")

emoji <- "\U0001f604"
iconv(emoji,, "latin1", sub = "Unicode") # "<U+1F604>"
iconv(emoji,, "latin1", sub = "c99")
}

\keyword{ character }
\keyword{ utilities }
