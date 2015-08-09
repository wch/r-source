% File src/library/base/man/paste.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{paste}
\title{Concatenate Strings}
\concept{combine strings}
\usage{
paste (\dots, sep = " ", collapse = NULL)
paste0(\dots, collapse = NULL)
}
\alias{paste}
\alias{paste0}
\arguments{
  \item{\dots}{one or more \R objects, to be converted to character vectors.}
  \item{sep}{a character string to separate the terms.  Not
    \code{\link{NA_character_}}.}
  \item{collapse}{an optional character string to separate the results.  Not
    \code{\link{NA_character_}}.}
}
\description{
  Concatenate vectors after converting to character.
}
\details{
  \code{paste} converts its arguments (\emph{via}
  \code{\link{as.character}}) to character strings, and concatenates
  them (separating them by the string given by \code{sep}).  If the
  arguments are vectors, they are concatenated term-by-term to give a
  character vector result.  Vector arguments are recycled as needed,
  with zero-length arguments being recycled to \code{""}.

  Note that \code{paste()} coerces \code{\link{NA_character_}}, the
  character missing value, to \code{"NA"} which may seem
  undesirable, e.g., when pasting two character vectors, or very
  desirable, e.g.\sspace{}in \code{paste("the value of p is ", p)}.

  \code{paste0(\dots, collapse)} is equivalent to
  \code{paste(\dots, sep = "", collapse)}, slightly more efficiently.

  If a value is specified for \code{collapse}, the values in the result
  are then concatenated into a single string, with the elements being
  separated by the value of \code{collapse}.
}
\value{
  A character vector of the concatenated values.  This will be of length
  zero if all the objects are, unless \code{collapse} is non-NULL in
  which case it is a single empty string.

  If any input into an element of the result is in UTF-8 (and none are
  declared with encoding \code{"bytes"}, (see \code{\link{Encoding}}),
  that element will be in UTF-8, otherwise in the current encoding in
  which case the encoding of the element is declared if the current
  locale is either Latin-1 or UTF-8, at least one of the corresponding
  inputs (including separators) had a declared encoding and all inputs
  were either ASCII or declared.

  If an input into an element is declared with encoding \code{"bytes"},
  no translation will be done of any of the elements and the resulting
  element will have encoding \code{"bytes"}.  If \code{collapse} is
  non-NULL, this applies also to the second, collapsing, phase, but some
  translation may have been done in pasting object together in the first
  phase.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{toString}} typically calls \code{paste(*, collapse=", ")}.
  String manipulation with
  \code{\link{as.character}}, \code{\link{substr}}, \code{\link{nchar}},
  \code{\link{strsplit}}; further, \code{\link{cat}} which concatenates and
  writes to a file, and \code{\link{sprintf}} for C like string
  construction.

  \sQuote{\link{plotmath}} for the use of \code{paste} in plot annotation.
}
\examples{
## When passing a single vector, paste0 and paste work like as.character.
paste0(1:12)
paste(1:12)        # same
as.character(1:12) # same

## If you pass several vectors to paste0, they are concatenated in a
## vectorized way.
(nth <- paste0(1:12, c("st", "nd", "rd", rep("th", 9))))

## paste works the same, but separates each input with a space.
## Notice that the recycling rules make every input as long as the longest input.
paste(month.abb, "is the", nth, "month of the year.")
paste(month.abb, letters)

## You can change the separator by passing a sep argument
## which can be multiple characters.
paste(month.abb, "is the", nth, "month of the year.", sep = "_*_")

## To collapse the output into a single string, pass a collapse argument.
paste0(nth, collapse = ", ")

## For inputs of length 1, use the sep argument rather than collapse
paste("1st", "2nd", "3rd", collapse = ", ") # probably not what you wanted
paste("1st", "2nd", "3rd", sep = ", ")

## You can combine the sep and collapse arguments together.
paste(month.abb, nth, sep = ": ", collapse = "; ")

## Using paste() in combination with strwrap() can be useful
## for dealing with long strings.
(title <- paste(strwrap(
    "Stopping distance of cars (ft) vs. speed (mph) from Ezekiel (1930)",
    width = 30), collapse = "\n"))
plot(dist ~ speed, cars, main = title)
}
\keyword{character}
