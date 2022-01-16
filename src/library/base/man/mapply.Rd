% File src/library/base/man/mapply.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2021 R Core Team
% Copyright 2002-2021 The R Foundation
% Distributed under GPL 2 or later

\name{mapply}
\title{Apply a Function to Multiple List or Vector Arguments}
\alias{mapply}
\alias{.mapply}
\description{
  \code{mapply} is a multivariate version of \code{\link{sapply}}.
  \code{mapply} applies \code{FUN} to the first elements of each \dots
  argument, the second elements, the third elements, and so on.
  Arguments are recycled if necessary.

  \code{.mapply()} is a bare-bones version of \code{mapply()}, e.g., to be
  used in other functions.
}
\usage{
mapply(FUN, \dots, MoreArgs = NULL, SIMPLIFY = TRUE,
       USE.NAMES = TRUE)
.mapply(FUN, dots, MoreArgs)
}
\arguments{
  \item{FUN}{function to apply, found via \code{\link{match.fun}}.}
  \item{\dots}{arguments to vectorize over, will be recycled to common
  length (zero if one of them is).  See also \sQuote{Details}.}
  \item{dots}{\code{\link{list}} or \code{\link{pairlist}} of arguments to
    vectorize over, see \code{\dots} above.}
  \item{MoreArgs}{a list of other arguments to \code{FUN}.}
  \item{SIMPLIFY}{logical or character string; attempt to reduce the
    result to a vector, matrix or higher dimensional array; see
    the \code{simplify} argument of \code{\link{sapply}}.}
  \item{USE.NAMES}{logical; use the names of the first \dots argument, or
    if that is an unnamed character vector, use that vector as the names.}
}
\details{
  \code{mapply} calls \code{FUN} for the values of \code{\dots}
  (re-cycled to the length of the longest, unless any have length zero
  where recycling to zero length will return \code{list()}),
  followed by the arguments given in \code{MoreArgs}.  The arguments in
  the call will be named if \code{\dots} or \code{MoreArgs} are named.

  For the arguments in \code{\dots} (or components in \code{dots}) class specific
  subsetting (such as \code{\link{[}}) and \code{length} methods will be
  used where applicable.
}
\value{
  A \code{\link{list}}, or for \code{SIMPLIFY = TRUE}, a vector, array or list.
}
\seealso{
  \code{\link{sapply}}, after which \code{mapply()} is modelled.

  \code{\link{outer}}, which applies a vectorized function to all
  combinations of two arguments.
}
\examples{
mapply(rep, 1:4, 4:1)

mapply(rep, times = 1:4, x = 4:1)

mapply(rep, times = 1:4, MoreArgs = list(x = 42))

mapply(function(x, y) seq_len(x) + y,
       c(a =  1, b = 2, c = 3),  # names from first
       c(A = 10, B = 0, C = -10))

word <- function(C, k) paste(rep.int(C, k), collapse = "")
## names from the first, too:
utils::str(L <- mapply(word, LETTERS[1:6], 6:1, SIMPLIFY = FALSE))

mapply(word, "A", integer()) # gave Error, now list()
}
\keyword{manip}
\keyword{utilities}
