% File src/library/utils/man/format.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{format}
\alias{formatUL}
\alias{formatOL}
\title{Format Unordered and Ordered Lists}
\description{
  Format unordered (itemize) and ordered (enumerate) lists.
}
\usage{
formatUL(x, label = "*", offset = 0,
         width = 0.9 * getOption("width"))
formatOL(x, type = "arabic", offset = 0, start = 1,
         width = 0.9 * getOption("width"))
}
\arguments{
  \item{x}{a character vector of list items.}
  \item{label}{a character string used for labelling the items.}
  \item{offset}{a non-negative integer giving the offset (indentation)
    of the list.}
  \item{width}{a positive integer giving the target column for wrapping
    lines in the output.}
  \item{type}{a character string specifying the \sQuote{type} of the
    labels in the ordered list.  If \code{"arabic"} (default), arabic
    numerals are used.  For \code{"Alph"} or \code{"alph"}, single upper
    or lower case letters are employed (in this case, the number of the
    last item must not exceed 26).  Finally, for \code{"Roman"} or
    \code{"roman"}, the labels are given as upper or lower case roman
    numerals (with the number of the last item maximally 3899).
    \code{type} can be given as a unique abbreviation of the above, or
    as one of the \acronym{HTML} style tokens \code{"1"} (arabic),
    \code{"A"}/\code{"a"} (alphabetic), or \code{"I"}/\code{"i"}
    (roman), respectively.}
  \item{start}{a positive integer specifying the starting number of the
    first item in an ordered list.}
}
\value{
  A character vector with the formatted entries.
}
\seealso{
  \code{\link{formatDL}} for formatting description lists.
}
\examples{
## A simpler recipe.
x <- c("Mix dry ingredients thoroughly.",
       "Pour in wet ingredients.",
       "Mix for 10 minutes.",
       "Bake for one hour at 300 degrees.")
## Format and output as an unordered list.
writeLines(formatUL(x))
## Format and output as an ordered list.
writeLines(formatOL(x))
## Ordered list using lower case roman numerals.
writeLines(formatOL(x, type = "i"))
## Ordered list using upper case letters and some offset.
writeLines(formatOL(x, type = "A", offset = 5))
}
\keyword{print}
