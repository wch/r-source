% file labels.Rd
% copyright (C) 1998 B. D. Ripley
%
\name{labels}
\title{Find Labels from Object}
\usage{
labels(object, ...)
labels.default(object, ...)
labels.terms(object, ...)
labels.lm(object, ...)
}
\alias{labels}
\alias{labels.default}
\alias{labels.terms}
\alias{labels.lm}
\arguments{
  \item{object}{Any \R object: the function is generic.}
}
\description{
 Find a suitable set of labels from an object for use in printing or
 plotting, for example.
}
\value{
  A character vector or list of such vectors. For a vector the results
  is the names or \code{seq(along=x)}, for a data frame or array it is
  the dimnames (with \code{NULL} expanded to \code{seq(len=d[i])}), for
  a \code{terms} object it is the term labels and for an \code{lm}
  object it is the term labels for estimable terms.
}
\author{B.D. Ripley}
\keyword{print}
\keyword{models}
