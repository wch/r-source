% File src/library/base/man/strsplit.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{strsplit}
\alias{strsplit}
\title{Split the Elements of a Character Vector}
\description{
  Split the elements of a character vector \code{x} into substrings
  according to the matches to substring \code{split} within them.
}
\usage{
strsplit(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE)
}
\arguments{
  \item{x}{
    character vector, each element of which is to be split.  Other
    inputs, including a factor, will give an error.
  }
  \item{split}{
    character vector (or object which can be coerced to such)
    containing \link{regular expression}(s) (unless \code{fixed = TRUE})
    to use for splitting.  If empty matches occur, in particular if
    \code{split} has length 0, \code{x} is split into single characters.
    If \code{split} has length greater than 1, it is re-cycled along
    \code{x}.
  }
  \item{fixed}{
    logical.  If \code{TRUE} match \code{split} exactly, otherwise
    use regular expressions.  Has priority over \code{perl}.
  }
  \item{perl}{logical.  Should perl-compatible regexps be used?}
  \item{useBytes}{logical.  If \code{TRUE} the matching is done
    byte-by-byte rather than character-by-character, and inputs with
    marked encodings are not converted.  This is forced (with a warning)
    if any input is found which is marked as \code{"bytes"}.}
}
\details{
  Argument \code{split} will be coerced to character, so
  you will see uses with \code{split = NULL} to mean
  \code{split = character(0)}, including in the examples below.

  Note that splitting into single characters can be done \emph{via}
  \code{split = character(0)} or \code{split = ""}; the two are
  equivalent.  The definition of \sQuote{character} here depends on the
  locale: in a single-byte locale it is a byte, and in a multi-byte
  locale it is the unit represented by a \sQuote{wide character} (almost
  always a Unicode code point).

  A missing value of \code{split} does not split the corresponding
  element(s) of \code{x} at all.

  The algorithm applied to each input string is
\preformatted{    repeat \{
        if the string is empty
            break.
        if there is a match
            add the string to the left of the match to the output.
            remove the match and all to the left of it.
        else
            add the string to the output.
            break.
    \}
}
  Note that this means that if there is a match at the beginning of a
  (non-empty) string, the first element of the output is \code{""}, but
  if there is a match at the end of the string, the output is the same
  as with the match removed.

  Invalid inputs in the current locale are warned about up to 5 times.
}
\value{
  A list of the same length as \code{x}, the \code{i}-th element of which
  contains the vector of splits of \code{x[i]}.

  If any element of \code{x} or \code{split} is declared to be in UTF-8
  (see \code{\link{Encoding}}), all non-ASCII character strings in the
  result will be in UTF-8 and have their encoding declared as UTF-8.
  For \code{perl = TRUE, useBytes = FALSE} all non-ASCII strings in a
  multibyte locale are translated to UTF-8.
}

\note{
  Prior to \R 2.11.0 there was an argument \code{extended} which could
  be used to select \sQuote{basic} regular expressions: this was often
  used when \code{fixed = TRUE} would be preferable.
}

\seealso{
  \code{\link{paste}} for the reverse,
  \code{\link{grep}} and \code{\link{sub}} for string search and
  manipulation; also \code{\link{nchar}}, \code{\link{substr}}.

  \sQuote{\link{regular expression}} for the details of the pattern
  specification.
}
\examples{
noquote(strsplit("A text I want to display with spaces", NULL)[[1]])

x <- c(as = "asfef", qu = "qwerty", "yuiop[", "b", "stuff.blah.yech")
# split x on the letter e
strsplit(x, "e")

unlist(strsplit("a.b.c", "."))
## [1] "" "" "" "" ""
## Note that 'split' is a regexp!
## If you really want to split on '.', use
unlist(strsplit("a.b.c", "[.]"))
## [1] "a" "b" "c"
## or
unlist(strsplit("a.b.c", ".", fixed = TRUE))

## a useful function: rev() for strings
strReverse <- function(x)
        sapply(lapply(strsplit(x, NULL), rev), paste, collapse = "")
strReverse(c("abc", "Statistics"))

## get the first names of the members of R-core
a <- readLines(file.path(R.home("doc"),"AUTHORS"))[-(1:8)]
a <- a[(0:2)-length(a)]
(a <- sub(" .*","", a))
# and reverse them
strReverse(a)

## Note that final empty strings are not produced:
strsplit(paste(c("", "a", ""), collapse="#"), split="#")[[1]]
# [1] ""  "a"
## and also an empty string is only produced before a definite match:
strsplit("", " ")[[1]]    # character(0)
strsplit(" ", " ")[[1]]   # [1] ""
}
\keyword{character}
