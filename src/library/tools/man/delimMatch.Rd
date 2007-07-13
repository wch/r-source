\name{delimMatch}
\alias{delimMatch}
\title{Delimited Pattern Matching}
\description{
  Match delimited substrings in a character vector, with proper nesting.
}
\usage{
delimMatch(x, delim = c("{", "}"), syntax = "Rd", multi.line = FALSE)
}
\arguments{
  \item{x}{a character vector.}
  \item{delim}{a character vector of length 2 giving the start and end
    delimiters.  Future versions might allow for arbitrary regular
    expressions.}
  \item{syntax}{currently, always the string \samp{"Rd"} indicating Rd
    syntax (i.e., \samp{\%} starts a comment extending till the end of
    the line, and \samp{\\} escapes).  Future versions might know about
    other syntaxes, perhaps via \dQuote{syntax tables} allowing to
    flexibly specify comment, escape, and quote characters.}
  \item{multi.line}{if \code{TRUE}, a single multi-line match is attempted.}
}
\value{
  By default, an integer vector of the same length as \code{x} giving the starting
  position (in characters) of the first match, or \eqn{-1} if there is
  none, with attribute \code{"match.length"} giving the length (in
  characters) of the matched text (or \eqn{-1} for no match).
  
  If \code{multi.line} is \code{TRUE}, the value is a length 4 integer vector
  giving with entries being the starting line, starting position, ending line, 
  ending position.
}
\seealso{
  \code{\link{regexpr}} for \dQuote{simple} pattern matching.
}
\examples{
x <- c("\\value{foo}", "function(bar)")
delimMatch(x)
delimMatch(x, c("(", ")"))
x <- c("before, { the start,", "the end },", "after.")
pos <- delimMatch(x, multi.line = TRUE)
subRange(x, pos)
}
\keyword{character}
