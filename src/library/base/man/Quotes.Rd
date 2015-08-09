% File src/library/base/man/Quotes.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{Quotes}
\alias{Quotes}
\alias{backtick}
\alias{backquote}
\alias{'}%'
\alias{"}%"
\alias{`}%`
\concept{quotes}
\concept{backslash}
\title{Quotes}
\description{
  Descriptions of the various uses of quoting in \R.
}
\details{
  Three types of quotes are part of the syntax of \R: single and double
  quotation marks and the backtick (or back quote, \samp{`}).  In
  addition, backslash is used to escape the following character
  inside character constants.
}
\section{Character constants}{
  Single and double quotes delimit character constants.  They can be used
  interchangeably but double quotes are preferred (and character
  constants are printed using double quotes), so single quotes are
  normally only used to delimit character constants containing double
  quotes.

  Backslash is used to start an escape sequence inside character
  constants.  Escaping a character not in the following table is an
  error.

  Single quotes need to be escaped by backslash in single-quoted
  strings, and double quotes in double-quoted strings.

  \tabular{ll}{
    \samp{\\n}\tab newline\cr
    \samp{\\r}\tab carriage return\cr
    \samp{\\t}\tab tab\cr
    \samp{\\b}\tab backspace\cr
    \samp{\\a}\tab alert (bell)\cr
    \samp{\\f}\tab form feed\cr
    \samp{\\v}\tab vertical tab\cr
    \samp{\\\\}\tab backslash \samp{\\}\cr
    \samp{\\'}\tab ASCII apostrophe \samp{'}\cr
    \samp{\\"}\tab ASCII quotation mark \samp{"}\cr
    \samp{\\`}\tab ASCII grave accent (backtick) \samp{`}\cr
    \samp{\\nnn}\tab character with given octal code (1, 2 or 3 digits)\cr
    \samp{\\xnn}\tab character with given hex code (1 or 2 hex digits)\cr
    \samp{\\unnnn}\tab Unicode character with given code (1--4 hex digits)\cr
    \samp{\\Unnnnnnnn}\tab Unicode character with given code (1--8 hex digits)\cr
  }
  Alternative forms for the last two are \samp{\\u\{nnnn\}} and
  \samp{\\U\{nnnnnnnn\}}.  All except the Unicode escape sequences are
  also supported when reading character strings by \code{\link{scan}}
  and \code{\link{read.table}} if \code{allowEscapes = TRUE}.  Unicode
  escapes can be used to enter Unicode characters not in the current
  locale's charset (when the string will be stored internally in UTF-8).

  The parser does not allow the use of both octal/hex and Unicode
  escapes in a single string.

  These forms will also be used by \code{\link{print.default}}
  when outputting non-printable characters (including backslash).

  Embedded nuls are not allowed in character strings, so using escapes
  (such as \samp{\\0}) for a nul will result in the string being
  truncated at that point (usually with a warning).
}
\section{Names and Identifiers}{
  Identifiers consist of a sequence of letters, digits, the period
  (\code{.}) and the underscore.  They must not start with a digit nor
  underscore, nor with a period followed by a digit.  \link{Reserved}
  words are not valid identifiers.

  The definition of a \emph{letter} depends on the current locale, but
  only ASCII digits are considered to be digits.

  Such identifiers are also known as \emph{syntactic names} and may be used
  directly in \R code.  Almost always, other names can be used
  provided they are quoted.  The preferred quote is the backtick
  (\samp{`}), and \code{\link{deparse}} will normally use it, but under
  many circumstances single or double quotes can be used (as a character
  constant will often be converted to a name).  One place where
  backticks may be essential is to delimit variable names in formulae:
  see \code{\link{formula}}.
}
\seealso{
  \code{\link{Syntax}} for other aspects of the syntax.

  \code{\link{sQuote}} for quoting English text.

  \code{\link{shQuote}} for quoting OS commands.

  The  \sQuote{R Language Definition} manual.
}
\examples{%% NOTE: Quote the \ even  "once more" !
'single quotes can be used more-or-less interchangeably'
"with double quotes to create character vectors"

## Single quotes inside single-quoted strings need backslash-escaping.
## Ditto double quotes inside double-quoted strings.
##
identical('"It\'s alive!", he screamed.',
          "\"It's alive!\", he screamed.") # same

## Backslashes need doubling, or they have a special meaning.
x <- "In ALGOL, you could do logical AND with /\\\\."
print(x)      # shows it as above ("input-like")
writeLines(x) # shows it as you like it ;-)

## Single backslashes followed by a letter are used to denote
## special characters like tab(ulator)s and newlines:
x <- "long\tlines can be\nbroken with newlines"
writeLines(x) # see also ?strwrap

## Backticks are used for non-standard variable names.
## (See make.names and ?Reserved for what counts as
## non-standard.)
`x y` <- 1:5
`x y`
d <- data.frame(`1st column` = rchisq(5, 2), check.names = FALSE)
d$`1st column`

## Backslashes followed by up to three numbers are interpreted as
## octal notation for ASCII characters.
"\110\145\154\154\157\40\127\157\162\154\144\41"

## \x followed by up to two numbers is interpreted as
## hexadecimal notation for ASCII characters.
(hw1 <- "\x48\x65\x6c\x6c\x6f\x20\x57\x6f\x72\x6c\x64\x21")

## Mixing octal and hexadecimal in the same string is OK
(hw2 <- "\110\x65\154\x6c\157\x20\127\x6f\162\x6c\144\x21")

## \u is also hexadecimal, but supported up to 4 numbers,
## using Unicode specification.  In the previous example,
## you can simply replace \x with \u.
(hw3 <- "\u48\u65\u6c\u6c\u6f\u20\u57\u6f\u72\u6c\u64\u21")

## The last three are all identical to
hw <- "Hello World!"
stopifnot(identical(hw, hw1), identical(hw1, hw2), identical(hw2, hw3))

## Using Unicode makes more sense for non-latin characters.
(nn <- "\u0126\u0119\u1114\u022d\u2001\u03e2\u0954\u0f3f\u13d3\u147b\u203c")

## Mixing \x and \u throws a _parse_ error (which is not catchable!)
\dontrun{
  "\x48\u65\x6c\u6c\x6f\u20\x57\u6f\x72\u6c\x64\u21"
}
##   -->   Error: mixing Unicode and octal/hex escapes .....

## \U works like \u, but supports up to eight numbers.
## So we can replace \u with \U in the previous example.
n2 <- "\U0126\U0119\U1114\U022d\U2001\U03e2\U0954\U0f3f\U13d3\U147b\U203c"
stopifnot(identical(nn, n2))

## Under systems supporting multi-byte locales (and not Windows),
## \U also supports the rarer characters outside the usual 16^4 range.
## See the R language manual,
## https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Literal-constants
## and bug 16098 https://bugs.r-project.org/bugzilla3/show_bug.cgi?id=16098
"\U1d4d7" # On Windows this gives the incorrect value of "\Ud4d7"

## nul characters (for terminating strings in C) are not allowed (parse errors)
\dontrun{% as above, these errors cannot be caught via try*(..)
  "foo\0bar"     # Error: nul character not allowed (line 1)
  "foo\u0000bar" # same error
}
}
\keyword{documentation}
