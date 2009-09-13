% File src/library/base/man/grep.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{grep}
\title{Pattern Matching and Replacement}
\alias{grep}
\alias{grepl}
\alias{sub}
\alias{gsub}
\alias{regexpr}
\alias{gregexpr}
\description{
  \code{grep}, \code{grepl}, \code{regexpr} and \code{gregexpr} search
  for matches to argument \code{pattern} within a character vector: they
  differ in the format of and amount of detail in the results.

  \code{sub} and \code{gsub} perform replacement of the first and all
  matches respectively.
}
\usage{
grep(pattern, x, ignore.case = FALSE, extended = TRUE,
     perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE,
     invert = FALSE)

grepl(pattern, x, ignore.case = FALSE, extended = TRUE,
     perl = FALSE, fixed = FALSE, useBytes = FALSE)

sub(pattern, replacement, x,
    ignore.case = FALSE, extended = TRUE, perl = FALSE,
    fixed = FALSE, useBytes = FALSE)

gsub(pattern, replacement, x,
     ignore.case = FALSE, extended = TRUE, perl = FALSE,
     fixed = FALSE, useBytes = FALSE)

regexpr(pattern, text, ignore.case = FALSE, extended = TRUE,
        perl = FALSE, fixed = FALSE, useBytes = FALSE)

gregexpr(pattern, text, ignore.case = FALSE, extended = TRUE,
         perl = FALSE, fixed = FALSE, useBytes = FALSE)
}
\arguments{
  \item{pattern}{character string containing a \link{regular expression}
    (or character string for \code{fixed = TRUE}) to be matched
    in the given character vector.  Coerced by
    \code{\link{as.character}} to a character string if possible.  If a
    character vector of length 2 or more is supplied, the first element
    is used with a warning.  Missing values are allowed except for
    \code{regexpr} and \code{gregexpr}.}
  \item{x, text}{a character vector where matches are sought, or an
    object which can be coerced by \code{as.character} to a character vector.}
  \item{ignore.case}{if \code{FALSE}, the pattern matching is \emph{case
      sensitive} and if \code{TRUE}, case is ignored during matching.}
  \item{extended}{if \code{TRUE}, extended regular expression matching
    is used, and if \code{FALSE} basic regular expressions are used: the
    latter are deprecated.}
  \item{perl}{logical. Should perl-compatible regexps be used?
    Has priority over \code{extended}.}
  \item{value}{if \code{FALSE}, a vector containing the (\code{integer})
    indices of the matches determined by \code{grep} is returned, and if
    \code{TRUE}, a vector containing the matching elements themselves is
    returned.}
  \item{fixed}{logical.  If \code{TRUE}, \code{pattern} is a string to be
    matched as is.  Overrides all conflicting arguments.}
  \item{useBytes}{logical.  If \code{TRUE} the matching is done
    byte-by-byte rather than character-by-character.  See
    \sQuote{Details}.}
  \item{invert}{logical.  If \code{TRUE} return indices or values for
         elements that do \emph{not} match.}
  \item{replacement}{a replacement for matched pattern in \code{sub} and
    \code{gsub}.  Coerced to character if possible.  For \code{fixed =
      FALSE} this can include backreferences \code{"\\1"} to
    \code{"\\9"} to parenthesized subexpressions of \code{pattern}.  For
    \code{perl = TRUE} only, it can also contain \code{"\\U"} or
    \code{"\\L"} to convert the rest of the replacement to upper or
    lower case and \code{"\\E"} to end case conversion.  If a
    character vector of length 2 or more is supplied, the first element
    is used with a warning.  If \code{NA}, all elements in the result
    corresponding to matches will be set to \code{NA}.
  }
}
\details{
  Arguments which should be character strings or character vectors are
  coerced to character if possible.

  Each of these functions operates in one of four modes:
  \enumerate{
    \item \code{fixed = TRUE}: use exact matching.
    \item \code{perl = TRUE}: use Perl-style regular expressions.
    \item \code{extended = FALSE}: use POSIX 1003.2 basic regular
    expressions (this mode is deprecated).
    \item Defaults for the above three arguments: use POSIX 1003.2
    extended regular expressions.
  }
  See the help pages on \link{regular expression} for details of the
  different types of regular expressions.

  The two \code{*sub} functions differ only in that \code{sub} replaces
  only the first occurrence of a \code{pattern} whereas \code{gsub}
  replaces all occurrences.

  For \code{regexpr} and \code{regexpr} it is an error for
  \code{pattern} to be \code{NA}, otherwise \code{NA} is permitted and
  gives an \code{NA} match.

  The main effect of \code{useBytes} is to avoid errors/warnings about
  invalid inputs and spurious matches in multibyte locales, but for
  \code{regexpr} it changes the interpretation of the output.  As from
  \R 2.10.0 it inhibits the conversion of inputs with marked encodings.
  
  Caseless matching does not make much sense for bytes in a multibyte
  locale, and you should expect it only to work for ASCII characters if
  \code{useBytes = TRUE}.
}
\value{
  \code{grep(value = FALSE)} returns an integer vector of the indices
  of the elements of \code{x} that yielded a match (or not, for
  \code{invert = TRUE}.
  
  \code{grep(value = TRUE)} returns a character vector containing the
  selected elements of \code{x} (after coercion, preserving names but no
  other attributes).

  \code{grepl} returns a logical vector (match or not for each element of
  \code{x}).

  For \code{sub} and \code{gsub} return a character vector of the same
  length and with the same attributes as \code{x} (after possible
  coercion).  Elements of character vectors \code{x} which are not
  substituted will be return unchanged (including any declared
  encoding).  If \code{useBytes = FALSE} the result will be in UTF-8 in
  a multibyte locale.

  For \code{regexpr} an integer vector of the same length as \code{text}
  giving the starting position of the first match or \eqn{-1} if there
  is none, with attribute \code{"match.length"} giving the length of the
  matched text (or \eqn{-1} for no match).  In a multi-byte locale the
  match lengths are in characters rather than bytes unless
  \code{useBytes = TRUE} is used.

  For \code{gregexpr} a list of the same length as \code{text} each
  element of which is an integer vector as in \code{regexpr}, except
  that the starting positions of every (disjoint) match are given.
}
\section{Warning}{
  The basic and extended modes of \code{gsub} and \code{gregexpr} do not
  work correctly with repeated word-boundaries (e.g. \code{pattern =
  "\\b"}).  Use \code{perl = TRUE} for such matches (but that may not
  works as expected with non-ASCII inputs, as the meaning of
  \sQuote{word} is system-dependent).
}
\source{
  The C code for POSIX-style regular expression matching has changed
  over the years. As from \R 2.10.0 the TRE library of Ville Laurikari
  (\url{http://laurikari.net/tre/}) is used.  Frox 2005 to \R 2.9.2,
  code based on \code{glibc} was used (and earlier code from GNU
  \command{grep}).  The POSIX standard does give some room for
  interpretation, especially in the handling of invalid regular
  expressions, so the results will have changed slightly.

  For Perl-style matching PCRE (\url{http://www.pcre.org}) is used.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole (\code{grep})
}
% the `aka' below is for ESS
\seealso{
  \link{regular expression} (aka \code{\link{regexp}}) for the details
  of the pattern specification.

  \code{\link{glob2rx}} to turn wildcard matches into regular expressions.

  \code{\link{agrep}} for approximate matching.

  \code{\link{tolower}}, \code{\link{toupper}} and \code{\link{chartr}}
  for character translations.
  \code{\link{charmatch}}, \code{\link{pmatch}}, \code{\link{match}}.
  \code{\link{apropos}} uses regexps and has more examples.
}
\examples{
grep("[a-z]", letters)

txt <- c("arm","foot","lefroo", "bafoobar")
if(length(i <- grep("foo",txt)))
   cat("'foo' appears at least once in\n\t",txt,"\n")
i # 2 and 4
txt[i]

## Double all 'a' or 'b's;  "\\" must be escaped, i.e., 'doubled'
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

## Note that in locales such as en_US this includes B as the
## collation order is aAbBcCdEe ...
(ot <- sub("[b-e]",".", txt))
txt[ot != gsub("[b-e]",".", txt)]#- gsub does "global" substitution

txt[gsub("g","#", txt) !=
    gsub("g","#", txt, ignore.case = TRUE)] # the "G" words

regexpr("en", txt)

gregexpr("e", txt)

## trim trailing white space
str <- 'Now is the time      '
sub(' +$', '', str)  ## spaces only
sub('[[:space:]]+$', '', str) ## white space, POSIX-style
sub('\\\\s+$', '', str, perl = TRUE) ## Perl-style white space

## capitalizing
txt <- "a test of capitalizing"
gsub("(\\\\w)(\\\\w*)", "\\\\U\\\\1\\\\L\\\\2", txt, perl=TRUE)
gsub("\\\\b(\\\\w)",    "\\\\U\\\\1",       txt, perl=TRUE)

txt2 <- "useRs may fly into JFK or laGuardia"
gsub("(\\\\w)(\\\\w*)(\\\\w)", "\\\\U\\\\1\\\\E\\\\2\\\\U\\\\3", txt2, perl=TRUE)
 sub("(\\\\w)(\\\\w*)(\\\\w)", "\\\\U\\\\1\\\\E\\\\2\\\\U\\\\3", txt2, perl=TRUE)
}
\keyword{character}
\keyword{utilities}
