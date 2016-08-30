% File src/library/base/man/abbreviate.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Copyright 2008-2016 The R Foundation
% Distributed under GPL 2 or later

\name{abbreviate}
\title{Abbreviate Strings}
\usage{
abbreviate(names.arg, minlength = 4, use.classes = TRUE,
           dot = FALSE, strict = FALSE,
           method = c("left.kept", "both.sides"), named = TRUE)
}
\alias{abbreviate}
\arguments{
  \item{names.arg}{a character vector of names to be abbreviated, or an
    object to be coerced to a character vector by \code{\link{as.character}}.}
  \item{minlength}{the minimum length of the abbreviations.}
  \item{use.classes}{logical: should lowercase characters be removed first?}
  \item{dot}{logical: should a dot (\code{"."}) be appended?}
  \item{strict}{logical: should \code{minlength} be observed strictly?
    Note that setting \code{strict = TRUE} may return \emph{non}-unique
    strings.}
  \item{method}{a character string specifying the method used with default
    \code{"left.kept"}, see \sQuote{Details} below.  Partial matches
    allowed.}
  \item{named}{logical: should \code{names} (with original vector) be returned.}
}
\description{
  Abbreviate strings to at least \code{minlength} characters,
  such that they remain \emph{unique} (if they were),
  unless \code{strict = TRUE}.
}
\details{
  The default algorithm (\code{method = "left.kept"}) used is similar
  to that of S.  For a single string it works as follows.
  First spaces at the ends of the string are stripped.
  Then (if necessary) any other spaces are stripped.
  Next, lower case vowels are removed followed by lower case consonants.
  Finally if the abbreviation is still longer than \code{minlength}
  upper case letters and symbols are stripped.

  Characters are always stripped from the end of the strings first.  If
  an element of \code{names.arg} contains more than one word (words are
  separated by spaces) then at least one letter from each word will be
  retained.

  Missing (\code{NA}) values are unaltered.

  If \code{use.classes} is \code{FALSE} then the only distinction is to
  be between letters and space.
}
\value{
  A character vector containing abbreviations for the character strings
  in its first argument.  Duplicates in the original \code{names.arg}
  will be given identical abbreviations.  If any non-duplicated elements
  have the same \code{minlength} abbreviations then, if \code{method =
  "both.sides"} the basic internal \code{abbreviate()} algorithm is
  applied to the characterwise \emph{reversed} strings; if there are
  still duplicated abbreviations and if \code{strict = FALSE} as by
  default, \code{minlength} is incremented by one and new abbreviations
  are found for those elements only.  This process is repeated until all
  unique elements of \code{names.arg} have unique abbreviations.

  If \code{names} is true, the character version of \code{names.arg} is
  attached to the returned value as a \code{\link{names}} attribute: no
  other attributes are retained.

  If a input element contains non-ASCII characters, the corresponding
  value will be in UTF-8 and marked as such (see \code{\link{Encoding}}).
}
\section{Warning}{
  If \code{use.classes} is true (the default), this is really only
  suitable for English, and prior to \R 3.3.0 did not work correctly
  with non-ASCII characters in multibyte locales.  It will warn if used
  with non-ASCII characters (and required to reduce the length).

  As from \R 3.3.0 the concept of \sQuote{vowel} is extended from
  English vowels by including characters which are accented versions of
  lower-case English vowels (including \sQuote{o with stroke}).  Of
  course, there are languages (even Western European languages such as
  Welsh) with other vowels.
}
\seealso{
  \code{\link{substr}}.
}
\examples{
x <- c("abcd", "efgh", "abce")
abbreviate(x, 2)
abbreviate(x, 2, strict = TRUE) # >> 1st and 3rd are == "ab"

(st.abb <- abbreviate(state.name, 2))
stopifnot(identical(unname(st.abb),
           abbreviate(state.name, 2, named=FALSE)))
table(nchar(st.abb)) # out of 50, 3 need 4 letters :
as <- abbreviate(state.name, 3, strict = TRUE)
as[which(as == "Mss")]
\dontshow{stopifnot(which(as == "Mss") == c(21,24,25))
}
## and without distinguishing vowels:
st.abb2 <- abbreviate(state.name, 2, FALSE)
cbind(st.abb, st.abb2)[st.abb2 != st.abb, ]

## method = "both.sides" helps:  no 4-letters, and only 4 3-letters:
st.ab2 <- abbreviate(state.name, 2, method = "both")
table(nchar(st.ab2))
## Compare the two methods:
cbind(st.abb, st.ab2)
}
\keyword{character}
