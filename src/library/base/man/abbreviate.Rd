\name{abbreviate}
\title{Abbreviate Strings}
\usage{
abbreviate(names.arg, minlength = 4, use.classes = TRUE,
           dot = FALSE)
}
\alias{abbreviate}
\arguments{
  \item{names.arg}{a character vector of names to be abbreviated, or an
    object to be coerced to a character vector by \code{\link{as.character}}.}
  \item{minlength}{the minimum length of the abbreviations.}
  \item{use.classes}{logical (currently ignored by \R).}
  \item{dot}{logical: should a dot (\code{"."}) be appended?}
}
\description{
  Abbreviate strings to at least \code{minlength} characters,
  such that they remain \emph{unique} (if they were).
}
\details{
  The algorithm used is similar to that of S.  For a single string it
  works as follows.
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
  the same \code{minlength} abbreviations then \code{minlength} is
  incremented by one and new abbreviations are found for those elements
  only.  This process is repeated until all unique elements of
  \code{names.arg} have unique abbreviations.

  The character version of \code{names.arg} is attached to the returned
  value as a names argument: no other attributes are retained.
}
\section{Warning}{
  This is really only suitable for English, and does not work correctly with
  non-ASCII characters in multibyte locales.  It will warn if used with
  non-ASCII characters.
}
\seealso{
  \code{\link{substr}}.
}
\examples{
x <- c("abcd", "efgh", "abce")
abbreviate(x, 2)

(st.abb <- abbreviate(state.name, 2))
table(nchar(st.abb))# out of 50, 3 need 4 letters
}
\keyword{character}
