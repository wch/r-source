\name{abbreviate}
\title{Abbreviate Strings}
\usage{
abbreviate(names.arg, minlength = 4, use.classes = TRUE,
           dot = FALSE)
}
\alias{abbreviate}
\arguments{
  \item{names.arg}{a vector of names to be abbreviated.}
  \item{minlength}{the minimum length of the abbreviations.}
  \item{use.classes}{a flag which is ignored by R.}
  \item{dot}{a flag indicating whether to postpend a dot.}
}
\description{
  The algorithm used is similar to that of S.
  First spaces at the beginning of the word are stripped.
  Then any other spaces are stripped.
  Next lower case vowels are removed followed by lower case consonants.
  Finally if the abbreviation is still longer than \code{minlength}
  upper case letters are stripped.

  Letters are always stripped from the end of the word first.
  If an element of \code{names.arg} contains more than one word (words
  are separted by space) then at least one letter from each word will be
  retained.
  If a single string is passed it is abbreviated in the same manner as a
  vector of strings.

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
  value as a names argument.
}
\seealso{
  \code{\link{substr}}.
}
\examples{
x <- c("abcd", "efgh", "abce")
abbreviate(x, 2)
}
\keyword{character}
