% File src/library/base/man/print.default.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{print.default}
\alias{print.default}
\description{
  \code{print.default} is the \emph{default} method of the generic
  \code{\link{print}} function which prints its argument.
}
\title{Default Printing}
\usage{
\method{print}{default}(x, digits = NULL, quote = TRUE,
      na.print = NULL, print.gap = NULL, right = FALSE,
      max = NULL, width = NULL, useSource = TRUE, \dots)
}
\arguments{
  \item{x}{the object to be printed.}
  \item{digits}{a non-null value for \code{digits} specifies the minimum
    number of significant digits to be printed in values.  The default,
    \code{NULL}, uses \code{\link{getOption}("digits")}.  (For the
    interpretation for complex numbers see \code{\link{signif}}.)
    Non-integer values will be rounded down, and only values
    greater than or equal to 1 and no greater than 22 are accepted.
  }
  \item{quote}{logical, indicating whether or not strings
    (\code{\link{character}}s) should be printed with surrounding quotes.}
  \item{na.print}{a character string which is used to indicate
    \code{\link{NA}} values in printed output, or \code{NULL}
    (see \sQuote{Details}).}
  \item{print.gap}{a non-negative integer \eqn{\le 1024},
    or \code{NULL} (meaning 1), giving the spacing between adjacent
    columns in printed vectors, matrices and arrays.}
  \item{right}{logical, indicating whether or not strings should be
    right aligned.  The default is left alignment.}
  \item{max}{a non-null value for \code{max} specifies the approximate
    maximum number of entries to be printed.  The default, \code{NULL},
    uses \code{\link{getOption}("max.print")}; see that help page for more
    details.}
  \item{width}{controls the maximum number of columns on a line used in
    printing vectors, matrices, etc.  The default, \code{NULL}, uses
    \code{\link{getOption}("digits")}; see that help page for more details.}
  \item{useSource}{logical, indicating whether to use source
    references or copies rather than deparsing \link{language objects}.
    The default is to use the original source if it is available.}
  \item{\dots}{further arguments to be passed to or from other
    methods.  They are ignored in this function.}
}
\details{
  The default for printing \code{NA}s is to print \code{NA} (without
  quotes) unless this is a character \code{NA} \emph{and} \code{quote =
    FALSE}, when \samp{<NA>} is printed.

  The same number of decimal places is used throughout a vector.  This
  means that \code{digits} specifies the minimum number of significant
  digits to be used, and that at least one entry will be encoded with
  that minimum number.  However, if all the encoded elements then have
  trailing zeroes, the number of decimal places is reduced until at
  least one element has a non-zero final digit.  Decimal points are only
  included if at least one decimal place is selected.

  Attributes are printed respecting their class(es), using the values of
  \code{digits} to \code{print.default}, but using the default values
  (for the methods called) of the other arguments.

  Option \code{width} controls the printing of vectors, matrices and
  arrays, and option \code{deparse.cutoff} controls the printing of
  \link{language objects} such as calls and formulae.

  When the \pkg{methods} package is attached, \code{print} will call
  \code{\link{show}} for \R objects with formal classes (\sQuote{S4})
  if called with no optional arguments.
}
\section{Large number of digits}{
  Note that for large values of \code{digits}, currently for
  \code{digits >= 16}, the calculation of the number of significant
  digits will depend on the platform's internal (C library)
  implementation of \samp{sprintf()} functionality.
}
\section{Single-byte locales}{
  If a non-printable character is encountered during output, it is
  represented as one of the ANSI escape sequences (\samp{\\a}, \samp{\\b},
  \samp{\\f}, \samp{\\n}, \samp{\\r}, \samp{\\t}, \samp{\\v}, \samp{\\\\} and
  \samp{\\0}: see \link{Quotes}), or failing that as a 3-digit octal
  code: for example the UK currency pound sign in the C locale (if
  implemented correctly) is printed as \samp{\\243}.  Which characters
  are non-printable depends on the locale.
  (Because some versions of Windows get this wrong, all bytes with the
  upper bit set are regarded as printable on Windows in a single-byte
  locale.)
}

#ifdef unix
\section{Unicode and other multi-byte locales}{
  In all locales, the characters in the ASCII range (\samp{0x00} to
  \samp{0x7f}) are printed in the same way, as-is if printable, otherwise
  via ANSI escape sequences or 3-digit octal escapes as described for
  single-byte locales.

  Multi-byte non-printing characters are printed as an escape sequence
  of the form \samp{\\uxxxx} or \samp{\\Uxxxxxxxx} (in hexadecimal).
  This is the internal code for the wide-character representation of the
  character.  If this is not known to be Unicode code points, a warning is
  issued.  The only known exceptions are certain Japanese ISO 2022
  locales on commercial Unixes, which use a concatenation of the bytes:
  it is unlikely that \R compiles on such a system.
% https://www.cl.cam.ac.uk/~mgk25/ucs/iso2022-wc.html

  It is possible to have a character string in a character vector that
  is not valid in the current locale.  If a byte is encountered that is
  not part of a valid character it is printed in hex in the form
  \samp{\\xab} and this is repeated until the start of a valid character.
  (This will rapidly recover from minor errors in UTF-8.)
}
#endif

#ifdef windows
\section{Multi-byte locales}{
  In all locales, the characters in the ASCII range (\samp{0x00} to
  \samp{0x7f}) are printed in the same way, as-is if printable, otherwise
  via ANSI escape sequences or 3-digit octal escapes as described for
  single-byte locales.

  Multi-byte non-printing characters are printed as an escape sequence
  of the form \samp{\\uxxxx} (in hexadecimal). This is the Unicode code
  point of the character.

  It is possible to have a character string in a character vector that
  is not valid in the current locale.  If a byte is encountered that is
  not part of a valid character it is printed in hex in the form
  \samp{\\xab} and this is repeated until the start of a valid character.
}
#endif

\seealso{
  The generic \code{\link{print}}, \code{\link{options}}.
  The \code{"\link{noquote}"} class and print method.

  \code{\link{encodeString}}, which encodes a character vector the way
  it would be printed.
}
\examples{
pi
print(pi, digits = 16)
LETTERS[1:16]
print(LETTERS, quote = FALSE)

M <- cbind(I = 1, matrix(1:10000, ncol = 10,
                         dimnames = list(NULL, LETTERS[1:10])))
utils::head(M)        # makes more sense than
print(M, max = 1000)  # prints 90 rows and a message about omitting 910
}
\keyword{print}
