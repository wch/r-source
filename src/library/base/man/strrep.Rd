% File src/library/base/man/strrep.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2015 R Core Team
% Distributed under GPL 2 or later

\name{strrep}
\alias{strrep}
\title{Repeat the Elements of a Character Vector}
\description{
  Repeat the character strings in a character vector a given number of
  times (i.e., concatenate the respective numbers of copies of the
  strings).
}
\usage{
strrep(x, times)
}
\arguments{
  \item{x}{a character vector, or an object which can be coerced to a
    character vector using \code{as.character}.}
  \item{times}{an integer vector giving the (non-negative) numbers of
    times to repeat the respective elements of \code{x}.}
}
\details{
  The elements of \code{x} and \code{times} will be recycled as
  necessary (if one has no elements, and empty character vector is
  returned).  Missing elements in \code{x} or \code{times} result in
  missing elements of the return value.
}
\value{
  A character vector with the elements of the given character vector
  repeated the given numbers of times.
}
\examples{
strrep("ABC", 2)
strrep(c("A", "B", "C"), 1 : 3)
## Create vectors with the given numbers of spaces:
strrep(" ", 1 : 5)
}
\keyword{character}
