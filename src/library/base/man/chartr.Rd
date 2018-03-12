% File src/library/base/man/chartr.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2009 R Core Team
% Distributed under GPL 2 or later

\name{chartr}
\alias{chartr}
\alias{tolower}
\alias{toupper}
\alias{casefold}
\title{Character Translation and Casefolding}
\description{
  Translate characters in character vectors, in particular from upper to
  lower case or vice versa.
}
\usage{
chartr(old, new, x)
tolower(x)
toupper(x)
casefold(x, upper = FALSE)
}
\arguments{
  \item{x}{a character vector, or an object that can be coerced to
    character by \code{\link{as.character}}.}
  \item{old}{a character string specifying the characters to be
    translated.  If a character vector of length 2 or more is supplied,
    the first element is used with a warning.}
  \item{new}{a character string specifying the translations. If a
    character vector of length 2 or more is supplied, the first element
    is used with a warning.}
  \item{upper}{logical: translate to upper or lower case?.}
}
\details{
  \code{chartr} translates each character in \code{x} that is specified
  in \code{old} to the corresponding character specified in \code{new}.
  Ranges are supported in the specifications, but character classes and
  repeated characters are not.  If \code{old} contains more characters
  than new, an error is signaled; if it contains fewer characters, the
  extra characters at the end of \code{new} are ignored.

  \code{tolower} and \code{toupper} convert upper-case characters in a
  character vector to lower-case, or vice versa.  Non-alphabetic
  characters are left unchanged.

  \code{casefold} is a wrapper for \code{tolower} and \code{toupper}
  provided for compatibility with S-PLUS.
}
\value{
  A character vector of the same length and with the same attributes as
  \code{x} (after possible coercion).

  Elements of the result will be have the encoding declared as that of
  the current locale (see \code{\link{Encoding}}) if the corresponding
  input had a declared encoding and the current locale is either Latin-1
  or UTF-8.  The result will be in the current locale's encoding unless
  the corresponding input was in UTF-8, when it will be in UTF-8 when
  the system has Unicode wide characters.
}
\seealso{
  \code{\link{sub}} and \code{\link{gsub}} for other
  substitutions in strings.
}
\examples{
x <- "MiXeD cAsE 123"
chartr("iXs", "why", x)
chartr("a-cX", "D-Fw", x)
tolower(x)
toupper(x)

## "Mixed Case" Capitalizing - toupper( every first letter of a word ) :

.simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1, 1)), substring(s, 2),
          sep = "", collapse = " ")
}
.simpleCap("the quick red fox jumps over the lazy brown dog")
## ->  [1] "The Quick Red Fox Jumps Over The Lazy Brown Dog"

## and the better, more sophisticated version:
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}
capwords(c("using AIC for model selection"))
## ->  [1] "Using AIC For Model Selection"
capwords(c("using AIC", "for MODEL selection"), strict = TRUE)
## ->  [1] "Using Aic"  "For Model Selection"
##                ^^^        ^^^^^
##               'bad'       'good'

## -- Very simple insecure crypto --
rot <- function(ch, k = 13) {
   p0 <- function(...) paste(c(...), collapse = "")
   A <- c(letters, LETTERS, " '")
   I <- seq_len(k); chartr(p0(A), p0(c(A[-I], A[I])), ch)
}

pw <- "my secret pass phrase"
(crypw <- rot(pw, 13)) #-> you can send this off

## now ``decrypt'' :
rot(crypw, 54 - 13) # -> the original:
stopifnot(identical(pw, rot(crypw, 54 - 13)))
}
\keyword{character}
