% File src/library/utils/man/aregexec.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2011-2 R Core Team
% Distributed under GPL 2 or later

\name{aregexec}
\alias{aregexec}
\title{Approximate String Match Positions}
\description{
  Determine positions of approximate string matches.
}
\usage{
aregexec(pattern, text, max.distance = 0.1, costs = NULL,
         ignore.case = FALSE, fixed = FALSE, useBytes = FALSE)
}
\arguments{
  \item{pattern}{a non-empty character string or a character string
    containing a regular expression (for \code{fixed = FALSE}) to be
    matched.
    Coerced by \code{\link{as.character}} to a string if possible.}
  \item{text}{character vector where matches are sought.
    Coerced by \code{\link{as.character}} to a character vector if
    possible.}
  \item{max.distance}{maximum distance allowed for a match.
    See \code{\link{agrep}}.}
  \item{costs}{cost of transformations.
    See \code{\link{agrep}}.}
  \item{ignore.case}{a logical.  If \code{TRUE}, case is ignored for
    computing the distances.}
  \item{fixed}{If \code{TRUE}, the pattern is matched literally (as is).
    Otherwise (default), it is matched as a regular expression.}
  \item{useBytes}{a logical.  If \code{TRUE} comparisons are
    byte-by-byte rather than character-by-character.}
}
\details{
  \code{aregexec} provides a different interface to approximate string
  matching than \code{\link{agrep}} (along the lines of the interfaces
  to exact string matching provided by \code{\link{regexec}} and
  \code{\link{grep}}).

  Note that by default, \code{\link{agrep}} performs literal matches,
  whereas \code{aregexec} performs regular expression matches.

  See \code{\link{agrep}} and \code{\link{adist}} for more information
  about approximate string matching and distances.

  Comparisons are byte-by-byte if \code{pattern} or any element of
  \code{text} is marked as \code{"bytes"}.
}
\value{
  A list of the same length as \code{text}, each element of which is
  either \eqn{-1} if there is no match, or a sequence of integers with
  the starting positions of the match and all substrings corresponding
  to parenthesized subexpressions of \code{pattern}, with attribute
  \code{"match.length"} an integer vector giving the lengths of the
  matches (or \eqn{-1} for no match).
}
\seealso{
  \code{\link{regmatches}} for extracting the matched substrings.
}
\examples{
## Cf. the examples for agrep.
x <- c("1 lazy", "1", "1 LAZY")
aregexec("laysy", x, max.distance = 2)
aregexec("(lay)(sy)", x, max.distance = 2)
aregexec("(lay)(sy)", x, max.distance = 2, ignore.case = TRUE)
m <- aregexec("(lay)(sy)", x, max.distance = 2)
regmatches(x, m)
}
\keyword{character}
