% File src/library/base/man/paste.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{paste}
\title{Concatenate Strings}
\concept{combine strings}
\usage{
paste(\dots, sep = " ", collapse = NULL)
}
\alias{paste}
\arguments{
  \item{\dots}{one or more \R objects, to be converted to character vectors.}
  \item{sep}{a character string to separate the terms.}
  \item{collapse}{an optional character string to separate the results.}
}
\description{
  Concatenate vectors after converting to character.
}
\details{
  \code{paste} converts its arguments (\emph{via}
  \code{\link{as.character}}) to character strings, and
  concatenates them (separating them by the string given by \code{sep}).
  If the arguments are vectors, they are concatenated term-by-term to
  give a character vector result.  Vector arguments are recycled as
  needed, with zero-length arguments being recycled to \code{""}.

  Note that \code{paste()} coerces \code{as.character(NA)} to \code{"NA"}
  (a string with two characters rather than the \code{\link{character}}
  missing value, \code{\link{NA_character_}}), which may seem
  undesirable, e.g., when pasting two character vectors, but very
  desirable in, e.g., \code{paste("the value of p is ", p)}.

  If a value is specified for \code{collapse}, the values in the result
  are then concatenated into a single string, with the elements being
  separated by the value of \code{collapse}.
}
\value{
  A character vector of the concatenated values.  This will be of length
  zero if all the objects are, unless \code{collapse} is non-NULL, in
  which case it is a single empty string.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  String manipulation with
  \code{\link{as.character}}, \code{\link{substr}}, \code{\link{nchar}},
  \code{\link{strsplit}}; further, \code{\link{cat}} which concatenates and
  writes to a file, and \code{\link{sprintf}} for C like string
  construction.

  \sQuote{\link{plotmath}} for the use of \code{paste} in plot annotation.
}
\examples{
paste(1:12) # same as as.character(1:12)
paste("A", 1:6, sep = "")
paste("Today is", date())
}
\keyword{character}
