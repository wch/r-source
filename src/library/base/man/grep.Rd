\name{grep}
\title{Pattern Matching and Replacement}
\usage{
grep(pattern, x, ignore.case=FALSE, extended=TRUE, value=FALSE)
sub(pattern, replacement, x,
        ignore.case=FALSE, extended=TRUE)
gsub(pattern, replacement, x,
        ignore.case=FALSE, extended=TRUE)
regexpr(pattern, text,  extended=TRUE)
}
\alias{grep}
\alias{sub}
\alias{gsub}
\alias{regexpr}
\arguments{
\item{pattern}{character string containing a regular expression
  to be matched in the vector of character string \code{vec}.}
\item{x, text}{a character vector where matches are sought.}
\item{ignore.case}{if \code{FALSE}, the pattern matching is
  \emph{case sensitive} and if \code{TRUE}, case is ignored during matching.}
\item{extended}{if \code{TRUE}, extended regular expression matching
  is used, and if \code{FALSE} basic regular expressions are used.}
\item{value}{if \code{FALSE}, a vector containing the (\code{integer}) indices
  of the matches determined by \code{grep} is returned,
  and if \code{TRUE}, a vector containing the matching
  elements themselves is returned.}
\item{replacement}{a replacement for matched pattern in
  \code{sub} and \code{gsub}.}
}
\description{
  \code{grep} searches for matches to \code{pattern} (its first
  argument) within the character vector \code{x} (second
  argument). \code{regexpr} does too, but returns more detail in a
  different format.

  \code{sub} and \code{gsub} perform replacement of matches
  determined by regular expression matching.
}
\details{
  The two \code{*sub} functions differ only in that \code{sub} replaces only
  the first occurrence of a \code{pattern} whereas \code{gsub} replaces
  all occurrences.

  The regular expressions used are those specified by POSIX 1003.2,
  either extended or basic, depending on the value of the
  \code{extended} argument.
}
\value{
    For \code{grep} a vector giving either the indices of the elements
    of \code{x} that yielded a match or, if \code{value} is \code{TRUE},
    the matched elements.

    For \code{sub} and \code{gsub} a character vector of the same
    length as the original.

    For \code{regexpr} an integer vector of the same length as
    \code{text} giving the starting position of the first match, or -1
    if there is none, with attribute \code{"match.length"} giving the
    length of the matched text (or -1 for no match).
}
\seealso{
  \code{\link{charmatch}}, \code{\link{pmatch}}, \code{\link{match}}.
  \code{\link{apropos}} uses regexps and has nice examples.
}
\examples{
grep("[a-z]", letters)

txt <- c("arm","foot","lefroo", "bafoobar")
if(any(i <- grep("foo",txt)))
   cat("`foo' appears at least once in\n\t",txt,"\n")
i # 2 and 4
txt[i]

## Double all 'a' or 'b's;  "\\" must be escaped, i.e. `doubled'
%% and escaped even once more in this *.Rd file!
gsub("([ab])", "\\\\1_\\\\1_", "abc and ABC")

txt <- c("The", "licenses", "for", "most", "software", "are",
  "designed", "to", "take", "away", "your", "freedom",
  "to", "share", "and", "change", "it.",
   "", "By", "contrast,", "the", "GNU", "General", "Public", "License",
   "is", "intended", "to", "guarantee", "your", "freedom", "to",
   "share", "and", "change", "free", "software", "--",
   "to", "make", "sure", "the", "software", "is",
   "free", "for", "all", "its", "users")
( i <- grep("[gu]", txt) ) # indices
stopifnot( txt[i] == grep("[gu]", txt, value = TRUE) )
(ot <- sub("[b-e]",".", txt))
txt[ot != gsub("[b-e]",".", txt)]#- gsub does "global" substitution

txt[gsub("g","#", txt) !=
    gsub("g","#", txt, ignore.case = TRUE)] # the "G" words

regexpr("en", txt)
}
\keyword{character}
\keyword{utilities}
