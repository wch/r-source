% File src/library/base/man/match.arg.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{match.arg}
\title{Argument Verification Using Partial Matching}
\description{
  \code{match.arg} matches a character \code{arg} against a table of
  candidate values as specified by \code{choices}.
}
\usage{
match.arg(arg, choices, several.ok = FALSE)
}
\alias{match.arg}
\arguments{
  \item{arg}{a character vector (of length one unless \code{several.ok}
    is \code{TRUE}) or \code{NULL} which means to take \code{choices[1]}.}
  \item{choices}{a character vector of candidate values, often missing, see
    \sQuote{Details}.}
  \item{several.ok}{either a \code{\link{logical}} specifying if \code{arg}
    should be allowed to have more than one element or a string, starting
    with \code{"all"} meaning that \code{arg} may have more than one
    element \emph{all} of which must match \code{choices}.}
}
\details{
  In the one-argument form \code{match.arg(arg)}, the choices are
  obtained from a default setting for the formal argument \code{arg} of
  the function from which \code{match.arg} was called.  (Since default
  argument matching will set \code{arg} to \code{choices}, this is
  allowed as an exception to the \sQuote{length one unless
    \code{several.ok} is \code{TRUE}} rule, and returns the first
  element.)

  Matching is done using \code{\link{pmatch}}, so \code{arg} may be
  abbreviated and the empty string (\code{""}) never matches, not even
  itself, see \code{\link{pmatch}}.

}
\value{
  The unabbreviated version of the exact or unique partial match if
  there is one; otherwise, an error is signalled if \code{several.ok} is
  false, as per default.  When \code{several.ok} is true and (at least)
  one element of \code{arg} has a match, all unabbreviated versions of
  matches are returned.
}
\section{Warning}{
  The error messages given are liable to change and did so in \R 4.2.0.
  Do not test them in packages.
}
\seealso{
  \code{\link{pmatch}},
  \code{\link{match.fun}},
  \code{\link{match.call}}.
}
\examples{
require(stats)
## Extends the example for 'switch'
center <- function(x, type = c("mean", "median", "trimmed")) {
  type <- match.arg(type)
  switch(type,
         mean = mean(x),
         median = median(x),
         trimmed = mean(x, trim = .1))
}
x <- rcauchy(10)
center(x, "t")       # Works
center(x, "med")     # Works
try(center(x, "m"))  # Error
stopifnot(identical(center(x),       center(x, "mean")),
          identical(center(x, NULL), center(x, "mean")) )

## Allowing more than one 'arg' and hence more than one match:
match.arg(c("gauss", "rect", "ep"),
          c("gaussian", "epanechnikov", "rectangular", "triangular"),
          several.ok = TRUE)
match.arg(c("a", ""),  c("", NA, "bb", "abc"), several.ok=TRUE) # |-->  "abc"

## New (2025-'26) option several.ok = "all" (or "all.match", "all_must_match", "all*" ..):
## --> Error if *some* of the several do not match:
abcd <- c("a","b","c","d")
try(match.arg(abcd, c("b", "c"), several.ok = FALSE)) # Error: ... 'arg' must be of length 1
    match.arg(abcd, c("b", "c"), several.ok = TRUE) # "b" "c"
try(match.arg(abcd, c("b", "c"), several.ok = "all")) # Error: ... should be one of "b", "c"
## i.e. _all_ of 'arg' must match the choices:
(m1 <- match.arg(abcd, letters, several.ok = TRUE       )) # "a" "b" "c" "d"
(m2 <- match.arg(abcd, letters, several.ok = "all.match")) #    ditto
stopifnot(identical(m1, abcd), identical(m1, m2),
          identical(abcd, match.arg(abcd, abcd, several.ok = TRUE)),
          identical(abcd, match.arg(abcd, abcd, several.ok = "all_must_match")))
}
\keyword{programming}
