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
  A character vector or list of such vectors. For a vector the results
  is the names or \code{seq(along=x)} and  for a data frame or array it is
  the dimnames (with \code{NULL} expanded to \code{seq(len=d[i])}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth \& Brooks/Cole.
}
\keyword{print}
\keyword{models}
