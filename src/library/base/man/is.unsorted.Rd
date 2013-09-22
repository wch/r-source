% File src/library/base/man/sort.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{is.unsorted}
\alias{is.unsorted}
\title{Test if an Object is Not Sorted}
\description{
  Test if an object is not sorted (in increasing order), without the
  cost of sorting it.
}
\usage{
is.unsorted(x, na.rm = FALSE, strictly = FALSE)
}
\arguments{
  \item{x}{an \R object with a class or a numeric, complex, character or
    logical vector.}
  \item{na.rm}{logical.  Should missing values be removed before checking?}
  \item{strictly}{logical indicating if the check should be for
    \emph{strictly} increasing values.}
}
\value{
  A length-one logical value.  All objects of length 0 or 1 are sorted:
  the result will be \code{NA} for objects of length 2 or more except for
  atomic vectors and objects with a class (where the \code{>=} or \code{>}
  method is used to compare \code{x[i]} with \code{x[i-1]} for
  \code{i} in \code{2:length(x)}).
}
\note{
  This function is designed for objects with one-dimensional indices, as
  described above.  Data frames, matrices and other arrays may give
  surprising results.
}
\seealso{\code{\link{sort}}, \code{\link{order}}.}

\keyword{univar}
