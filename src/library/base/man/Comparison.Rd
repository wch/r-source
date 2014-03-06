% File src/library/base/man/Comparison.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{Comparison}
\alias{<}
\alias{<=}
\alias{==}
\alias{!=}
\alias{>=}
\alias{>}
\alias{Comparison}
\alias{collation}
\title{Relational Operators}
\description{
  Binary operators which allow the comparison of values in atomic vectors.
}
\usage{
x < y
x > y
x <= y
x >= y
x == y
x != y
}
\arguments{
  \item{x, y}{atomic vectors, symbols, calls, or other objects for which
    methods have been written.}
}
\details{
  The binary comparison operators are generic functions: methods can be
  written for them individually or via the
  \code{\link[=S3groupGeneric]{Ops}}) group generic function.  (See
  \code{\link[=S3groupGeneric]{Ops}} for how dispatch is computed.)

  Comparison of strings in character vectors is lexicographic within the
  strings using the collating sequence of the locale in use: see
  \code{\link{locales}}.  The collating sequence of locales such as
  \samp{en_US} is normally different from \samp{C} (which should use
  ASCII) and can be surprising.  Beware of making \emph{any} assumptions
  about the collation order: e.g. in Estonian \code{Z} comes between
  \code{S} and \code{T}, and collation is not necessarily
  character-by-character -- in Danish \code{aa} sorts as a single
  letter, after \code{z}.  In Welsh \code{ng} may or may not be a single
  sorting unit: if it is it follows \code{g}.  Some platforms may
  not respect the locale and always sort in numerical order of the bytes
  in an 8-bit locale, or in Unicode code-point order for a UTF-8 locale (and
  may not sort in the same order for the same language in different
  character sets).  Collation of non-letters (spaces, punctuation signs,
  hyphens, fractions and so on) is even more problematic.

  Character strings can be compared  with different marked encodings
  (see \code{\link{Encoding}}): they are translated to UTF-8 before
  comparison.

  Raw vectors should not really be considered to have an order, but the
  numeric order of the byte representation is used.

  At least one of \code{x} and \code{y} must be an atomic vector, but if
  the other is a list \R attempts to coerce it to the type of the atomic
  vector: this will succeed if the list is made up of elements of length
  one that can be coerced to the correct type.

  If the two arguments are atomic vectors of different types, one is
  coerced to the type of the other, the (decreasing) order of precedence
  being character, complex, numeric, integer, logical and raw.

  Missing values (\code{\link{NA}}) and \code{\link{NaN}} values are
  regarded as non-comparable even to themselves, so comparisons
  involving them will always result in \code{NA}.  Missing values can
  also result when character strings are compared and one is not valid
  in the current collation locale.

  Language objects such as symbols and calls are deparsed to
  character strings before comparison.
}
\value{
  A logical vector indicating the result of the element by element
  comparison.  The elements of shorter vectors are recycled as
  necessary.

  Objects such as arrays or time-series can be compared this way
  provided they are conformable.
}
\note{
  Do not use \code{==} and \code{!=} for tests, such as in \code{if}
  expressions, where you must get a single \code{TRUE} or
  \code{FALSE}.  Unless you are absolutely sure that nothing unusual
  can happen, you should use the \code{\link{identical}} function
  instead.

  For numerical and complex values, remember \code{==} and \code{!=} do
  not allow for the finite representation of fractions, nor for rounding
  error.  Using \code{\link{all.equal}} with \code{identical} is almost
  always preferable.  See the examples.
}
\section{S4 methods}{
  These operators are members of the S4 \code{\link{Compare}} group generic,
  and so methods can be written for them individually as well as for the
  group generic (or the \code{Ops} group generic), with arguments
  \code{c(e1, e2)}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Collation of character strings is a complex topic.  For an
  introduction see
  \url{http://en.wikipedia.org/wiki/Collating_sequence}.  The
  \emph{Unicode Collation Algorithm}
  (\url{http://unicode.org/reports/tr10/}) is likely to be increasingly
  influential.  Where available \R makes use of ICU
  (\url{http://site.icu-project.org/} for collation.
}
\seealso{
  \code{\link{factor}} for the behaviour with factor arguments.

  \code{\link{Syntax}} for operator precedence.

  \code{\link{icuSetCollate}} to tune the string collation algorithm
  when ICU is in use.
}
\examples{
x <- stats::rnorm(20)
x < 1
x[x > 0]

x1 <- 0.5 - 0.3
x2 <- 0.3 - 0.1
x1 == x2                           # FALSE on most machines
identical(all.equal(x1, x2), TRUE) # TRUE everywhere

\donttest{
# range of most 8-bit charsets, as well as of Latin-1 in Unicode
z <- c(32:126, 160:255)
x <- if(l10n_info()$MBCS) {
    intToUtf8(z, multiple = TRUE)
} else rawToChar(as.raw(z), multiple= TRUE)
## by number
writeLines(strwrap(paste(x, collapse=" "), width = 60))
## by locale collation
writeLines(strwrap(paste(sort(x), collapse=" "), width = 60))
}}
\keyword{logic}
