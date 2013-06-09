% File src/library/base/man/abbreviate.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Copyright 2008      The R Foundation
% Distributed under GPL 2 or later

\name{abbreviate}
\title{Abbreviate Strings}
\usage{
abbreviate(names.arg, minlength = 4, use.classes = TRUE,
           dot = FALSE, strict = FALSE,
           method = c("left.kept", "both.sides"))
}
\alias{abbreviate}
\arguments{
  \item{names.arg}{a character vector of names to be abbreviated, or an
    object to be coerced to a character vector by \code{\link{as.character}}.}
  \item{minlength}{the minimum length of the abbreviations.}
  \item{use.classes}{logical (currently ignored by \R).}
  \item{dot}{logical: should a dot (\code{"."}) be appended?}
  \item{strict}{logical: should \code{minlength} be observed strictly?
    Note that setting \code{strict = TRUE} may return \emph{non}-unique
    strings.}
  \item{method}{a string specifying the method used with default
    \code{"left.kept"}, see \sQuote{Details} below.}
}
\description{
  Abbreviate strings to at least \code{minlength} characters,
  such that they remain \emph{unique} (if they were),
  unless \code{strict = TRUE}.
}
\details{
  The default algorithm (\code{method = "left.kept"}) used is similar
  to that of S.  For a single string it works as follows.
  First all spaces at the beginning of the string are stripped.
  Then (if necessary) any other spaces are stripped.
  Next, lower case vowels are removed (starting at the right)
  followed by lower case consonants.
  Finally if the abbreviation is still longer than \code{minlength}
  upper case letters are stripped.

  Characters are always stripped from the end of the word first.
  If an element of \code{names.arg} contains more than one word (words
  are separated by space) then at least one letter from each word will be
  retained.

  Missing (\code{NA}) values are unaltered.

  If \code{use.classes} is \code{FALSE} then the only distinction is to
  be between letters and space.  This has NOT been implemented.
}
\value{
  A character vector containing abbreviations for the strings in its
  first argument.  Duplicates in the original \code{names.arg} will be
  given identical abbreviations.  If any non-duplicated elements have
  the same \code{minlength} abbreviations then, if \code{method =
  "both.sides"} the basic internal \code{abbreviate()} algorithm is
  applied to the characterwise \emph{reversed} strings; if there are
  still duplicated abbreviations and if \code{strict = FALSE} as by
  default, \code{minlength} is incremented by one and new abbreviations
  are found for those elements only.  This process is repeated until all
  unique elements of \code{names.arg} have unique abbreviations.

  The character version of \code{names.arg} is attached to the returned
  value as a names argument: no other attributes are retained.
}
\section{Warning}{
  This is really only suitable for English, and does not work correctly with
  non-ASCII characters in multibyte locales.  It will warn if used with
  non-ASCII characters (and required to reduce the length).
}
\seealso{
  \code{\link{substr}}.
}
\examples{
x <- c("abcd", "efgh", "abce")
abbreviate(x, 2)
abbreviate(x, 2, strict = TRUE) # >> 1st and 3rd are == "ab"

(st.abb <- abbreviate(state.name, 2))
table(nchar(st.abb)) # out of 50, 3 need 4 letters :
as <- abbreviate(state.name, 3, strict = TRUE)
as[which(as == "Mss")]
\dontshow{stopifnot(which(as == "Mss") == c(21,24,25))
}
## method="both.sides" helps:  no 4-letters, and only 4 3-letters:
st.ab2 <- abbreviate(state.name, 2, method = "both")
table(nchar(st.ab2))
## Compare the two methods:
cbind(st.abb, st.ab2)
}
\keyword{character}
