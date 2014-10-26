% File src/library/base/man/agrep.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{agrep}
\alias{agrep}
\alias{agrepl}
\alias{fuzzy matching}
\alias{.amatch_bounds}
\alias{.amatch_costs}
\title{Approximate String Matching (Fuzzy Matching)}
\description{
  Searches for approximate matches to \code{pattern} (the first argument)
  within each element of the string \code{x} (the second argument) using
  the generalized Levenshtein edit distance (the minimal possibly
  weighted number of insertions, deletions and substitutions needed to
  transform one string into another).
}
\usage{
agrep(pattern, x, max.distance = 0.1, costs = NULL,
      ignore.case = FALSE, value = FALSE, fixed = TRUE,
      useBytes = FALSE)

agrepl(pattern, x, max.distance = 0.1, costs = NULL,
       ignore.case = FALSE, fixed = TRUE, useBytes = FALSE)
}
\arguments{
  \item{pattern}{a non-empty character string or a character string
    containing a regular expression (for \code{fixed = FALSE}) to be
    matched.
    Coerced by \code{\link{as.character}} to a string if possible.}
  \item{x}{character vector where matches are sought.
    Coerced by \code{\link{as.character}} to a character vector if
    possible.}
  \item{max.distance}{Maximum distance allowed for a match.  Expressed
    either as integer, or as a fraction of the \emph{pattern} length
    times the maximal transformation cost (will be replaced by the
    smallest integer not less than the corresponding fraction), or a
    list with possible components
    \describe{
      \item{\code{cost}:}{maximum number/fraction of match cost
	(generalized Levenshtein distance)}
      \item{\code{all}:}{maximal number/fraction of \emph{all}
	transformations (insertions, deletions and substitutions)}
      \item{\code{insertions}:}{maximum number/fraction of insertions}
      \item{\code{deletions}:}{maximum number/fraction of deletions}
      \item{\code{substitutions}:}{maximum number/fraction of
        substitutions}
    }
    If \code{cost} is not given, \code{all} defaults to 10\%, and the
    other transformation number bounds default to \code{all}.
    The component names can be abbreviated.
  }
  \item{costs}{a numeric vector or list with names partially matching
    \samp{insertions}, \samp{deletions} and \samp{substitutions} giving
    the respective costs for computing the generalized Levenshtein
    distance, or \code{NULL} (default) indicating using unit cost for
    all three possible transformations.
    Coerced to integer via \code{\link{as.integer}} if possible.}
  \item{ignore.case}{if \code{FALSE}, the pattern matching is \emph{case
      sensitive} and if \code{TRUE}, case is ignored during matching.}
  \item{value}{if \code{FALSE}, a vector containing the (integer)
    indices of the matches determined is returned and if \code{TRUE}, a
    vector containing the matching elements themselves is returned.}
  \item{fixed}{logical.  If \code{TRUE} (default), the pattern is
    matched literally (as is).  Otherwise, it is matched as a regular
    expression.}
  \item{useBytes}{logical. in a multibyte locale, should the comparison
    be character-by-character (the default) or byte-by-byte.}
}
\details{
  The Levenshtein edit distance is used as measure of approximateness:
  it is the (possibly cost-weighted) total number of insertions,
  deletions and substitutions required to transform one string into
  another.

  This uses \code{tre} by Ville Laurikari
  (\url{http://http://laurikari.net/tre/}), which supports MBCS
  character matching.

  The main effect of \code{useBytes} is to avoid errors/warnings about
  invalid inputs and spurious matches in multibyte locales.
  It inhibits the conversion of inputs with marked encodings, and is
  forced if any input is found which is marked as \code{"bytes"} (see
  \code{\link{Encoding}}).
}
\note{
  Since someone who read the description carelessly even filed a bug
  report on it, do note that this matches substrings of each element of
  \code{x} (just as \code{\link{grep}} does) and \bold{not} whole
  elements.  See also \code{\link{adist}} in package \pkg{utils}, which
  optionally returns the offsets of the matched substrings.
}
\value{
  \code{agrep} returns a vector giving the indices of the elements that
  yielded a match, or, if \code{value} is \code{TRUE}, the matched
  elements (after coercion, preserving names but no other attributes).

  \code{agrepl} returns a logical vector.
}
\author{
  Original version in \R < 2.10.0 by David Meyer.
  Current version by Brian Ripley and Kurt Hornik.
}
\seealso{
  \code{\link{grep}}, \code{\link{adist}}.
}
\examples{
agrep("lasy", "1 lazy 2")
agrep("lasy", c(" 1 lazy 2", "1 lasy 2"), max = list(sub = 0))
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2)
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2, value = TRUE)
agrep("laysy", c("1 lazy", "1", "1 LAZY"), max = 2, ignore.case = TRUE)
}
\keyword{character}
