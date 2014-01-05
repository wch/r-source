% File src/library/base/man/mapply.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Copyright 2002-2010 The R Foundation
% Distributed under GPL 2 or later

\name{mapply}
\alias{mapply}
\title{Apply a Function to Multiple List or Vector Arguments}
\description{
  \code{mapply} is a multivariate version of \code{\link{sapply}}.
  \code{mapply} applies \code{FUN} to the first elements of each \dots
  argument, the second elements, the third elements, and so on.
  Arguments are recycled if necessary.
}
\usage{
mapply(FUN, \dots, MoreArgs = NULL, SIMPLIFY = TRUE,
       USE.NAMES = TRUE)
}
\arguments{
  \item{FUN}{function to apply, found via \code{\link{match.fun}}.}
  \item{\dots}{arguments to vectorize over (vectors or lists of strictly
    positive length, or all of zero length).  See also \sQuote{Details}.}
  \item{MoreArgs}{a list of other arguments to \code{FUN}.}
  \item{SIMPLIFY}{logical or character string; attempt to reduce the
    result to a vector, matrix or higher dimensional array; see
    the \code{simplify} argument of \code{\link{sapply}}.}
  \item{USE.NAMES}{logical; use names if the first \dots argument has
    names, or if it is a character vector, use that character vector as
    the names.}
}
\details{
  \code{mapply} calls \code{FUN} for the values of \code{\dots}
  (re-cycled to the length of the longest, unless any have length zero),
  followed by the arguments given in \code{MoreArgs}.  The arguments in
  the call will be named if \code{\dots} or \code{MoreArgs} are named.

  Arguments with classes in \code{\dots} will be accepted, and their
  subsetting and \code{length} methods will be used.
}
\value{
  A list, or for \code{SIMPLIFY = TRUE}, a vector, array or list.
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
utils::str(mapply(word, LETTERS[1:6], 6:1, SIMPLIFY = FALSE))
}
\keyword{manip}
\keyword{utilities}
