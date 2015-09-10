% File src/library/base/man/nchar.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{nchar}
\alias{nchar}
\alias{nzchar}
\title{Count the Number of Characters (or Bytes or Width)}
\usage{
nchar(x, type = "chars", allowNA = FALSE, keepNA = NA)

nzchar(x, keepNA = FALSE)
}
\description{
  \code{nchar} takes a character vector as an argument and
  returns a vector whose elements contain the sizes of
  the corresponding elements of \code{x}.

  \code{nzchar} is a fast way to find out if elements of a character
  vector are non-empty strings.
}
\arguments{
  \item{x}{character vector, or a vector to be coerced to a character
    vector.  Giving a factor is an error.}
  \item{type}{character string: partial matching to one of
    \code{c("bytes", "chars", "width")}.  See \sQuote{Details}.}
  \item{allowNA}{logical: should \code{NA} be returned for invalid
    multibyte strings or \code{"bytes"}-encoded strings (rather than
    throwing an error)?}
  \item{keepNA}{logical: should \code{NA} be returned where ever
  \code{x} is \code{\link{NA}}?  If false, \code{nchar()} returns
  \code{2}, as that is the number of printing characters used when
  strings are written to output, and \code{nzchar()} is \code{TRUE}.  The
  default for \code{nchar()}, \code{NA}, means to use \code{keepNA = TRUE}
  unless \code{type} is \code{"width"}.  Used to be (implicitly) hard
  coded to \code{FALSE} in \R versions \eqn{\le}{<=} 3.2.0.}
}
\details{
  The \sQuote{size} of a character string can be measured in one of
  three ways (corresponding to the \code{type} argument):
  \describe{
    \item{\code{bytes}}{The number of bytes needed to store the string
      (plus in C a final terminator which is not counted).}
    \item{\code{chars}}{The number of human-readable characters.}
    \item{\code{width}}{The number of columns \code{\link{cat}} will use to
      print the string in a monospaced font.  The same as \code{chars}
      if this cannot be calculated.}
  }
  These will often be the same, and almost always will be in single-byte
  locales (but note how \code{type} determines the default for
  \code{keepNA}).  There will be differences between the first two with
  multibyte character sequences, e.g.\sspace{}in UTF-8 locales.

  The internal equivalent of the default method of
  \code{\link{as.character}} is performed on \code{x} (so there is no
  method dispatch).  If you want to operate on non-vector objects
  passing them through \code{\link{deparse}} first will be required.
}
\value{
  For \code{nchar}, an integer vector giving the sizes of each element.
  For missing values (i.e., \code{NA}, i.e., \code{\link{NA_character_}}),
  \code{nchar()} returns \code{\link{NA_integer_}} if \code{keepNA} is
  true, and \code{2}, the number of printing characters, if false.

  \code{type = "width"} gives (an approximation to) the number of
  columns used in printing each element in a terminal font, taking into
  account double-width, zero-width and \sQuote{composing} characters.
  
  If \code{allowNA = TRUE} and an element is detected as invalid in a
  multi-byte character set such as UTF-8, its number of characters and
  the width will be \code{NA}.  Otherwise the number of characters will
  be non-negative, so \code{!is.na(nchar(x, "chars", TRUE))} is a test
  of validity.

  A character string marked with \code{"bytes"} encoding (see
  \code{\link{Encoding}}) has a number of bytes, but neither a known
  number of characters nor a width, so the latter two types are
  \code{NA} if \code{allowNA = TRUE}, otherwise an error.

  Names, dims and dimnames are copied from the input.

  For \code{nzchar}, a logical vector of the same length as \code{x},
  true if and only if the element has non-zero length; if the element is
  \code{NA}, \code{nzchar()} is true when \code{keepNA} is false, as by
  default, and \code{NA} otherwise.
}
\note{
  This does \strong{not} by default give the number of characters that
  will be used to \code{print()} the string.  Use
  \code{\link{encodeString}} to find that.
#ifdef windows
  This is particularly important on Windows when \samp{\\uxxxx}
  sequences have been used to enter Unicode characters not representable
  in the current encoding.  Thus \code{nchar("\\u2642")} is \code{1},
  and it is printed in \code{Rgui} as one character, but it will be
  printed in \code{Rterm} as \code{<U+2642>}, which is what
  \code{encodeString} gives.
#endif
#ifdef unix
  Where character strings have been marked as UTF-8, the number of
  characters and widths will be computed in UTF-8, even though printing
  may use escapes such as \samp{<U+2642>} in a non-UTF-8 locale.
#endif
  
  The concept of \sQuote{width} is a slippery one even in a monospaced
  font. Some human languages have the concept of \emph{combining}
  characters, in which two or more characters are rendered together: an
  example would be \code{"y\u306"}, which is two characters of width
  one: combining characters are given width zero, and there are other
  zero-width characters such as the zero-width space \code{"\u200b"}.

  Some East Asian languages have \sQuote{wide} characters, ideographs
  which are conventionally printed across two columns when mixed with
  ASCII and other \sQuote{narrow} characters in those languages.  The
  problem is that whether a computer prints wide characters over two or
  one columns depends on the font, with it not being uncommon to use two
  columns in a font intended for East Asian users and a single column in
  a \sQuote{Western} font.  Unicode has encodings for \sQuote{fullwidth}
  versions of ASCII characters and \sQuote{halfwidth} versions of
  Katakana (Japanese) and Hangul (Korean) characters.  Then there is the
  \sQuote{East Asian Ambiguous class} (Greek, Cyrillic, signs, some
  accented Latin chars, etc), for which the historical practice was to
  use two columns in East Asia and one elsewhere.  The width quoted by
  \code{nchar} for characters in that class (and some others) depends on
  the locale, being one except in some East Asian locales on some OSes
  (notably Windows).

  Control characters are given width zero.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
  
  Unicode Standard Annex #11: \emph{East Asian Width.}
  \url{http://www.unicode.org/reports/tr11/}
}
\seealso{
  \code{\link{strwidth}} giving width of strings for plotting;
  \code{\link{paste}}, \code{\link{substr}}, \code{\link{strsplit}}
}
\examples{
x <- c("asfef", "qwerty", "yuiop[", "b", "stuff.blah.yech")
nchar(x)
# 5  6  6  1 15

nchar(deparse(mean))
# 18 17  <-- unless mean differs from base::mean

x[3] <- NA; x
nchar(x, keepNA= TRUE) #  5  6 NA  1 15
nchar(x, keepNA=FALSE) #  5  6  2  1 15
stopifnot(identical(nchar(x     ), nchar(x, keepNA= TRUE)),
          identical(nchar(x, "w"), nchar(x, keepNA=FALSE)),
          identical(is.na(x), is.na(nchar(x))))
}
\keyword{character}
