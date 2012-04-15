% File src/library/tools/man/delimMatch.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{delimMatch}
\alias{delimMatch}
\title{Delimited Pattern Matching}
\description{
  Match delimited substrings in a character vector, with proper nesting.
}
\usage{
delimMatch(x, delim = c("{", "}"), syntax = "Rd")
}
\arguments{
  \item{x}{a character vector.}
  \item{delim}{a character vector of length 2 giving the start and end
    delimiters.  Future versions might allow for arbitrary regular
    expressions.}
  \item{syntax}{currently, always the string \code{"Rd"} indicating Rd
    syntax (i.e., \samp{\%} starts a comment extending till the end of
    the line, and \samp{\\} escapes).  Future versions might know about
    other syntax, perhaps via \sQuote{syntax tables} allowing to
    flexibly specify comment, escape, and quote characters.}
}
\value{
  An integer vector of the same length as \code{x} giving the starting
  position (in characters) of the first match, or \eqn{-1} if there is
  none, with attribute \code{"match.length"} giving the length (in
  characters) of the matched text (or \eqn{-1} for no match).
}
\seealso{
  \code{\link{regexpr}} for \sQuote{simple} pattern matching.
}
\examples{
x <- c("\\\\value{foo}", "function(bar)")
delimMatch(x)
delimMatch(x, c("(", ")"))
}
\keyword{character}
