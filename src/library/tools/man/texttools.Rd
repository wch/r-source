% File src/library/tools/man/texttools.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2026 R Core Team
% Distributed under GPL 2 or later

\name{texttools}
\alias{gsub_with_transformed_matches}
\title{Text tools}
\description{
  Pattern replacement with transformed matches.
}
\usage{
gsub_with_transformed_matches(pattern, replacement, x, trafo, count, ...)
}
\arguments{
  \item{pattern, replacement, x}{as for \code{\link{gsub}}.}
  \item{trafo}{a single unary function, or a list of such functions.}
  \item{count}{a vector of integers giving the numbers of parenthesized
    subexpressions to be transformed (0 gives the whole match).}
  \item{...}{further arguments to \code{\link{gsub}}.}
}
\details{
  \code{gsub_with_transformed_matches()} replaces all occurrences of the
  given pattern with replacements featuring transformations of matches.
  Character string (\samp{\%s}) conversion specifications in
  \code{replacement} are replaced by applying the respective
  transformations in \code{trafo} to the respective matches
  (parenthesized subexpressions of \code{pattern}) specified by
  \code{count}.
}
\value{
  A character vector with the replaced matches.
}
\examples{
## Suppose we want to change <FOO@BaR> to <FOO@bar>.
## We can achieve this via a pattern using parenthesized subexpressions
## for the name and the domain parts, and invoke tolower() on the
## matches domain parts:
(s <- "A <B@C> D <E@F> G")
gsub_with_transformed_matches("<([^>]+)@([^>]+)>",
                              "<\\\\1@\%s>",
                              s,
                              tolower,
                              2L)
## In case we want to also uppercase the name part:
(s <- "A <b@C> D <E@f> G")
gsub_with_transformed_matches("<([^>]+)@([^>]+)>",
                              "<\%s@\%s>",
                              s,
                              list(toupper, tolower),
                              c(1L, 2L))
}
