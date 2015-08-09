% File src/library/base/man/grepRaw.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{grepRaw}
\title{Pattern Matching for Raw Vectors}
\alias{grepRaw}
\description{
  \code{grepRaw} searches for substring \code{pattern} matches within a
  raw vector \code{x}.
}
\usage{
grepRaw(pattern, x, offset = 1L, ignore.case = FALSE,
        value = FALSE, fixed = FALSE, all = FALSE, invert = FALSE)
}
\arguments{
  \item{pattern}{raw vector containing a \link{regular expression}
    (or fixed pattern for \code{fixed = TRUE}) to be matched in the
    given raw vector.  Coerced by \code{\link{charToRaw}} to a character
    string if possible.}
  \item{x}{a raw vector where matches are sought, or an object which can
    be coerced by \code{charToRaw} to a raw vector.  \link{Long vectors}
    are not supported.}
  \item{ignore.case}{if \code{FALSE}, the pattern matching is \emph{case
      sensitive} and if \code{TRUE}, case is ignored during matching.}
  \item{offset}{An integer specifying the offset from
    which the search should start.  Must be positive.  The beginning of
    line is defined to be at that offset so \code{"^"} will match there.}
  \item{value}{logical.  Determines the return value: see \sQuote{Value}.}
  \item{fixed}{logical.  If \code{TRUE}, \code{pattern} is a pattern to be
    matched as is.}
  \item{all}{logical.  If \code{TRUE} all matches are returned,
    otherwise just the first one.}
  \item{invert}{logical.  If \code{TRUE} return indices or values for
    elements that do \emph{not} match.  Ignored (with a warning) unless
    \code{value = TRUE}.}
}
\details{
  Unlike \code{\link{grep}}, seeks matching patterns within the raw
  vector \code{x} . This has implications especially in the \code{all =
  TRUE} case, e.g., patterns matching empty strings are inherently
  infinite and thus may lead to unexpected results.

  The argument \code{invert} is interpreted as asking to return the
  complement of the match, which is only meaningful for \code{value =
  TRUE}.  Argument \code{offset} determines the start of the search, not
  of the complement.  Note that \code{invert = TRUE} with \code{all =
  TRUE} will split \code{x} into pieces delimited by the pattern
  including leading and trailing empty strings (consequently the use of
  regular expressions with \code{"^"} or \code{"$"} in that case may
  lead to less intuitive results).

  Some combinations of arguments such as \code{fixed = TRUE} with
  \code{value = TRUE} are supported but are less meaningful.
}

\value{
  \code{grepRaw(value = FALSE)} returns an integer vector of the offsets
  at which matches have occurred.  If \code{all = FALSE} then it will be
  either of length zero (no match) or length one (first matching
  position).

  \code{grepRaw(value = TRUE, all = FALSE)} returns a raw vector which
  is either empty (no match) or the matched part of \code{x}.

  \code{grepRaw(value = TRUE, all = TRUE)} returns a (potentially
  empty) list of raw vectors corresponding to the matched parts.
}

\source{
  The TRE library of Ville Laurikari (\url{http://laurikari.net/tre/})
  is used except for \code{fixed = TRUE}.
}

% the `aka' below is for ESS
\seealso{
  \link{regular expression} (aka \code{\link{regexp}}) for the details
  of the pattern specification.

  \code{\link{grep}} for matching character vectors.
}
\keyword{utilities}
