\name{grep}
\title{Pattern Matching and Replacement}
\usage{
grep(pattern, x, ignore.case=FALSE, extended=TRUE, value=FALSE)
sub(pattern, replacement, x,
        ignore.case=FALSE, extended=TRUE)
gsub(pattern, replacement, x,
        ignore.case=FALSE, extended=TRUE)
}
\alias{grep}
\alias{sub}
\alias{gsub}
\arguments{
\item{pattern}{character string containing a regular expression
  to be matched in the vector of character string \code{vec}.}
\item{x}{a vector of character strings where matches are sought.}
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
argument) within the vector \code{x} of character strings (second argument).

\code{sub} and \code{gsub} perform replacement of matches
determined by regular expression matching.
The two functions differ only in that \code{sub} replaces only
the first occurence of a \code{pattern} whereas \code{gsub} replaces
all occurences.

The regular expressions used are those specified by POSIX 1003.2,
either extended or basic, depending on the value of the
\code{extended} argument.
}
\note{
This function is based on the \emph{regex} regular expression library
written by Henry Spencer of the University of Toronto.
}
\seealso{
    \code{\link{charmatch}}, \code{\link{pmatch}}, \code{\link{match}}.
    \code{\link{apropos}} uses regexps and has nice examples.
}
\examples{
grep("[a-z]", letters)

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
all( txt[i] == grep("[gu]", txt, value = TRUE) )
(ot <- sub("[b-e]",".", txt))
txt[ot != gsub("[b-e]",".", txt)]#- gsub does "global" substitution

txt[gsub("g","#", txt) !=
    gsub("g","#", txt, ignore.case = TRUE)] # the "G" words
}
\keyword{character}
\keyword{utilities}
