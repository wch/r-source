\name{grep}
\title{Pattern Matching and Replacement}
\alias{grep}
\alias{sub}
\alias{gsub}
\alias{regexpr}
\description{
  \code{grep} searches for matches to \code{pattern} (its first
  argument) within the character vector \code{x} (second
  argument).  \code{regexpr} does too, but returns more detail in a
  different format.

  \code{sub} and \code{gsub} perform replacement of matches determined
  by regular expression matching.
}
\usage{
grep(pattern, x, ignore.case = FALSE, extended = TRUE, perl = FALSE,
     value = FALSE, fixed = FALSE)
sub(pattern, replacement, x,
    ignore.case = FALSE, extended = TRUE, perl = FALSE)
gsub(pattern, replacement, x,
     ignore.case = FALSE, extended = TRUE, perl = FALSE)
regexpr(pattern, text,  extended = TRUE, perl = FALSE, fixed = FALSE)
}
\arguments{
  \item{pattern}{character string containing a \link{regular expression}
    (or character string for \code{fixed = TRUE}) to be matched
    in the given character vector.}
  \item{x, text}{a character vector where matches are sought.}
  \item{ignore.case}{if \code{FALSE}, the pattern matching is \emph{case
      sensitive} and if \code{TRUE}, case is ignored during matching.}
  \item{extended}{if \code{TRUE}, extended regular expression matching
    is used, and if \code{FALSE} basic regular expressions are used.}
  \item{perl}{logical. Should perl-compatible regexps be used?
    Has priority over \code{extended}.}
  \item{value}{if \code{FALSE}, a vector containing the (\code{integer})
    indices of the matches determined by \code{grep} is returned, and if
    \code{TRUE}, a vector containing the matching elements themselves is
    returned.}
  \item{fixed}{logical.  If \code{TRUE}, \code{pattern} is a string to be
    matched as is.  Overrides all other arguments.}
  \item{replacement}{a replacement for matched pattern in \code{sub} and
    \code{gsub}.}
}
\details{
  Arguments which should be character strings or character vectors are
  coerced to character if possible.

  The two \code{*sub} functions differ only in that \code{sub} replaces
  only the first occurrence of a \code{pattern} whereas \code{gsub}
  replaces all occurrences.

  For \code{regexpr} it is an error for \code{pattern} to be \code{NA},
  otherwise \code{NA} is permitted and matches only itself.

  The regular expressions used are those specified by POSIX 1003.2,
  either extended or basic, depending on the value of the
  \code{extended} argument, unless \code{perl = TRUE} when they are
  those of PCRE,
  \url{ftp://ftp.csx.cam.ac.uk/pub/software/programming/pcre/}.
  (The exact set of patterns supported may depend on the version of
  PCRE installed on the system in use.)
}
\value{
  For \code{grep} a vector giving either the indices of the elements of
  \code{x} that yielded a match or, if \code{value} is \code{TRUE}, the
  matched elements.

  For \code{sub} and \code{gsub} a character vector of the same length
  as the original.

  For \code{regexpr} an integer vector of the same length as \code{text}
  giving the starting position of the first match, or \eqn{-1} if there
  is none, with attribute \code{"match.length"} giving the length of the
  matched text (or \eqn{-1} for no match).
}
\section{Warning}{
  The standard regular-expression code has been reported to be very slow
  or give errors when applied to extremely long character strings
  (tens of thousands of characters or more): the code used when
  \code{perl=TRUE} seems faster and more reliable for such usages.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole (\code{grep})
}
\seealso{
  \link{regular expression} (aka \code{\link{regexp}}) for the details
% the `aka' above is for ESS (and ?reg....) where a space is problematic
  of the pattern specification.

  \code{\link{agrep}} for approximate matching.

  \code{\link{tolower}}, \code{\link{toupper}} and \code{\link{chartr}}
  for character translations.
  \code{\link{charmatch}}, \code{\link{pmatch}}, \code{\link{match}}.
  \code{\link{apropos}} uses regexps and has nice examples.
}
\examples{
grep("[a-z]", letters)

txt <- c("arm","foot","lefroo", "bafoobar")
if(any(i <- grep("foo",txt)))
   cat("'foo' appears at least once in\n\t",txt,"\n")
i # 2 and 4
txt[i]

## Double all 'a' or 'b's;  "\\" must be escaped, i.e., 'doubled'
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

## Note that is locales such as en_US this includes B as the the
## collation order is aAbBcCdEe ...
(ot <- sub("[b-e]",".", txt))
txt[ot != gsub("[b-e]",".", txt)]#- gsub does "global" substitution

txt[gsub("g","#", txt) !=
    gsub("g","#", txt, ignore.case = TRUE)] # the "G" words

regexpr("en", txt)

## trim trailing white space
str = 'Now is the time      '
sub(' +$', '', str)  ## spaces only
sub('[[:space:]]+$', '', str) ## white space, POSIX-style
sub('\\\\s+$', '', str, perl = TRUE) ## Perl-style white space
}
\keyword{character}
\keyword{utilities}
