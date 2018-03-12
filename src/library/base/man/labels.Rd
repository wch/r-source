% File src/library/base/man/labels.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{labels}
\title{Find Labels from Object}
\usage{
labels(object, \dots)
}
\alias{labels}
\alias{labels.default}
\arguments{
  \item{object}{Any \R object: the function is generic.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  Find a suitable set of labels from an object for use in printing or
  plotting, for example.  A generic function.
}
\value{
  A character vector or list of such vectors.  For a vector the results
  is the names or \code{seq_along(x)} and for a data frame or array it
  is the dimnames (with \code{NULL} expanded to \code{seq_len(d[i])}).
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth & Brooks/Cole.
}
\keyword{print}
\keyword{models}
