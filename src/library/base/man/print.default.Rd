\name{print.default}
\alias{print.default}
\description{
  \code{print.default} is the \emph{default} method of the generic
  \code{\link{print}} function which prints its argument.
}
\title{Default Printing}
\usage{
\method{print}{default}(x, digits = NULL, quote = TRUE, na.print = NULL,
      print.gap = NULL, right = FALSE, max = NULL, useSource = TRUE, \dots)
}
\arguments{
  \item{x}{the object to be printed.}
  \item{digits}{a non-null value for \code{digits} specifies the minimum
    number of significant digits to be printed in values.  The default,
    \code{NULL}, uses \code{\link{getOption}(digits)}.  (For the
    interpretation for complex numbers see \code{\link{signif}}.)
    Non-integer values will be rounded down, and only values
    greater than or equal to 1 and no greater than 22 are accepted.
  }
  \item{quote}{logical, indicating whether or not strings
    (\code{\link{character}}s) should be printed with surrounding quotes.}
  \item{na.print}{a character string which is used to indicate
    \code{\link{NA}} values in printed output, or \code{NULL} (see Details)}
  \item{print.gap}{a non-negative integer \eqn{\le 1024}{<= 1024},
    or \code{NULL} (meaning 1), giving the spacing between adjacent
    \dQuote{columns} in printed vectors, matrices and arrays.}
  \item{right}{logical, indicating whether or not strings should be
    right aligned.  The default is left alignment.}
  \item{max}{a non-null value for \code{max} specifies the approximate
    maximum number of entries to be printed.  The default, \code{NULL},
    uses \code{\link{getOption}(max.print)}; see that help page for more
    details.}
  \item{useSource}{logical, indicating whether to use source
    references or copies rather than deparsing language objects.
    The default is to use the original source if it is available.}
  \item{\dots}{further arguments to be passed to or from other
    methods.  They are ignored in this function.}
}
\details{
  The default for printing \code{NA}s is to print \code{NA} (without
  quotes) unless this is a character \code{NA} \emph{and} \code{quote =
    FALSE}, when \code{<NA>} is printed.

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

  When the \pkg{methods} package is attached, \code{print} will call
  \code{\link{show}} for \R objects with formal classes if called
  with no optional arguments.
}
\section{Warning}{
  Using too large a value of \code{digits} may lead to representation
  errors in the calculation of the number of significant digits and the
  decimal representation: these are likely for \code{digits >= 16}, and
  as from \R 2.5.0 these possible errors are taken into account in
  assessing the numher of significant digits to be printed in that case.

  Whereas earlier versions of \R might have printed further digits for
  \code{digits >= 16} on some platforms, they were not necessarily reliable.
}
\section{Single-byte locales}{
  If a non-printable character is encountered during output, it is
  represented as one of the ANSI escape sequences (\code{\a}, \code{\b},
  \code{\f}, \code{\n}, \code{\r}, \code{\t}, \code{\v}, \code{\\\\} and
  \code{\0}: see \link{Quotes}), or failing that as a 3-digit octal
  code: for example the UK currency pound sign in the C locale (if
  implemented correctly) is printed as \code{\243}.  Which characters
  are non-printable depends on the locale.
#ifdef windows
  (Because some versions of Windows get this wrong, all bytes with the
  upper bit set are regarded as printable on Windows in a single-byte
  locale.)
#endif
}
#ifdef unix
\section{Unicode and other multi-byte locales}{
#endif
#ifdef windows
\section{Multi-byte locales}{
#endif
  In all locales, the characters in the ASCII range (\code{0x00} to
  \code{0x7f}) are printed in the same way, as-is if printable, otherwise
  via ANSI escape sequences or 3-digit octal escapes as described for
  single-byte locales.

  Multi-byte non-printing characters are printed as an escape sequence
  of the form \code{\uxxxx}
#ifndef windows
  or \code{\Uxxxxxxxx}
#endif
  (in hexadecimal).
#ifdef windows
  This is the Unicode point of the character.
#endif
#ifdef unix
  This is the internal code for the wide-character representation of the
  character.  If this is not known to be the Unicode point, a warning is
  issued.  The only known exceptions are certain Japanese ISO2022
  locales on commercial Unixes, which use a concatenation of the bytes:
  it is unlikely that \R compiles on such a system.
% http://www.cl.cam.ac.uk/~mgk25/ucs/iso2022-wc.html
#endif

  It is possible to have a character string in a character vector that
  is not valid in the current locale.  If a byte is encountered that is
  not part of a valid character it is printed in hex in the form
  \code{\xab} and this is repeated until the start of a valid character.
#ifdef unix
  (This will rapidly recover from minor errors in UTF-8.)
#endif
}
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

M <- cbind(I=1, matrix(1:10000, ncol=10, dimnames=list(NULL, LETTERS[1:10])))
utils::head(M) # makes more sense than
print(M, max= 1000)# prints 90 rows and a message about omitting 910
}
\keyword{print}
