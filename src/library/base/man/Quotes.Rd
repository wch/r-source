\name{Quotes}
\alias{Quotes}
\alias{backtick}
\alias{`}%`
\concept{quotes}
\concept{backslash}
\title{Quotes}
\description{
  Outlines the various uses of quoting in \R.
}
\details{
  Three types of quote are part of the syntax of \R, single, double and
  back quotes.  In addition, backslash is used for quoting the following
  character(s) inside character constants.
}
\section{Character constants}{
  Single and double quotes delimit character constants.  They can be used
  interchangeably but double quotes are preferred (and character
  constants are printed using double quotes), so single quotes are
  normally only used to delimit character constants containing double
  quotes.

  Backslash is used as an escape sequence inside character constants.
  Unless specified in the follow table, an escaped character is
  interpreted as the character itself.
  Single quotes need to be escaped by backslash in single-quoted
  strings, and double quotes in double-quoted strings.

  \tabular{ll}{
    \code{\\n}\tab newline\cr
    \code{\\r}\tab carriage return\cr
    \code{\\t}\tab tab\cr
    \code{\\b}\tab backspace\cr
    \code{\\a}\tab alert (bell)\cr
    \code{\\f}\tab form feed\cr
    \code{\\v}\tab vertical tab\cr
    \code{\\\\}\tab backslash \code{\\}\cr
    \code{\\nnn}\tab character with given octal code (1, 2 or 3 digits)\cr
    \code{\\xnn}\tab character with given hex code (1 or 2 hex digits)\cr
    \code{\\unnnn}\tab Unicode character with given hex code\cr
    \code{\\Unnnnnnnn}\tab Unicode character with given hex code\cr
  }
  The last two are only supported in Unicode and other multibyte locales,
  and the last is not supported on Windows.  All but the Unicode escapes
  are also supported when reading character strings from a connection by
  \code{\link{scan}}. 
}
\section{Names and Identifiers}{
  Identifiers consist of a sequence of letters, digits, the period
  (\code{.}) and the underscore.  They must not start with a digit nor
  underscore, nor with a period followed by a digit.

  The definition of a \emph{letter} depends on the current locale, but
  only ASCII digits are considered to be digits.

  Such identifiers are also known as \emph{syntactic names} and may be used
  directly in \R code.  In many circumstances other names can be used
  provided they are quoted.  The preferred quote is the backtick
  (\code{`}), and \code{\link{deparse}} will normally use it, but under
  many circumstances single or double quotes can be used (as a character
  constant will be converted to a name).  One place where
  backticks may be essential is to delimit variable names in formulae:
  see \code{\link{formula}}.
}
\seealso{
  \code{\link{Syntax}} for other aspects of the syntax.

  \code{\link{sQuote}} for quoting English text.

  \code{\link{shQuote}} for quoting OS commands.
}
\keyword{documentation}
