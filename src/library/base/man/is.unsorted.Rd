% File src/library/base/man/sort.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
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
  \item{x}{an \R object with a class or a numeric, complex, character,
    logical or raw vector.}
  \item{na.rm}{logical.  Should missing values be removed before checking?}
  \item{strictly}{logical indicating if the check should be for
    \emph{strictly} increasing values.}
}
\value{
  A length-one logical value.  All objects of length 0 or 1 are sorted.
  Otherwise, the result will be \code{NA} except for atomic vectors and
  objects with an S3 class (where the \code{>=} or \code{>} method is
  used to compare \code{x[i]} with \code{x[i-1]} for \code{i} in
  \code{2:length(x)}) or with an S4 class where you have to provide a
  method for \code{\link{is.unsorted}()}.
%% FIXME: rather want  '>' and '>=' methods for S4 classes be sufficient
}
\note{
  This function is designed for objects with one-dimensional indices, as
  described above.  Data frames, matrices and other arrays may give
  surprising results.
}
\seealso{\code{\link{sort}}, \code{\link{order}}.}

\keyword{univar}
