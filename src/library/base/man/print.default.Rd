\name{print.default}
\alias{print.default}
\alias{print.matrix}
\description{
  \code{print.default} is the \emph{default} method of the generic
  \code{\link{print}} function which prints its argument.
}
\title{Default Printing}
\usage{
\method{print}{default}(x, digits = NULL, quote = TRUE, na.print = NULL,
      print.gap = NULL, right = FALSE, \dots)
}
\arguments{
  \item{x}{the object to be printed.}
  \item{digits}{a non-null value for \code{digits} specifies the minimum
    number of significant digits to be printed in values.  If
    \code{digits} is \code{NULL}, the value of \code{digits} set by
    \code{options} is used.}
  \item{quote}{logical, indicating whether or not strings
    (\code{\link{character}}s) should be printed with surrounding quotes.}
  \item{na.print}{a character string which is used to indicate
    \code{\link{NA}} values in printed output, or \code{NULL} (see Details)}
  \item{print.gap}{a non-negative integer \eqn{\le 1024}{<= 1024},
    giving the spacing between adjacent \dQuote{columns} in printed
    vectors, matrices and arrays, or \code{NULL} meaning 1.}
  \item{right}{logical, indicating whether or not strings should be
    right-aligned. The default is left-alignment.}
  \item{\dots}{further arguments to be passed to or from other
    methods. They are ignored in this function.}
}
\details{
  The default for printing \code{NA}s is to print \code{NA} (without
  quotes) unless this is a character \code{NA} \emph{and} \code{quote =
    FALSE}, when \code{<NA>} is printed.

  The same number of decimal places is used throughout a vector,  This
  means that \code{digits} specifies the minimum number of significant
  digits to be used, and that at least one entry will be printed with
  that minimum number.

  Attributes are printed respecting their class(es), using the values of
  \code{digits} to \code{print.default}, but using the default values
  (for the methods called) of the other arguments.

  When the \pkg{methods} package is attached, \code{print} will call
  \code{\link{show}} for \R objects with formal classes if called
  with no optional arguments.

  If a non-printable character is encountered during output, it is
  represented as one of the ANSI escape sequences (\code{\a}, \code{\b},
  \code{\f}, \code{\n}, \code{\r}, \code{\t}, \code{\v} and \code{\0}),
  or failing that as a 3-digit octal code: for example the UK currency
  pound in the C locale (if implemented correctly) is printed as
  \code{\243}.  Which characters are non-printable depends on the locale.
#ifdef windows
  (Because some versions of Windows get this wrong, all 8-bit characters
  are regarded as printable on Windows.)
#endif
}
\section{Unicode locales}{
  In a Unicode (UTF-8) locale, characters \code{0x00} to \code{0x1F} and
  \code{0x7f} (the ASCII non-printing characters) are printed in the
  same way , via ANSI escape sequences or 3-digit octal escapes.
  Multi-byte non-printing characters are printed with as an escape
  sequence of the form \code{\uxxxxxx} (in hexadecimal).

  It is possible to have a character string in an object that is not
  valid UTF-8.  If a byte is encountered that is not part of an
  encoded Unicode character it is printed in hex in the form \code{<xx>}
  and the next character is tried.
}
\note{
  \code{print.matrix} is currently identical to \code{print.default},
  but was prior to \code{1.7.0} did not print attributes and did not
  have a \code{digits} argument.  It is provided only because some
  packages call it explicitly.
}
\seealso{
  The generic \code{\link{print}}, \code{\link{options}}.
  The \code{"\link{noquote}"} class and print method.

  \code{\link{encodeString}}.
}
\examples{
pi
print(pi, digits = 16)
LETTERS[1:16]
print(LETTERS, quote = FALSE)
}
\keyword{print}
