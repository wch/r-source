\name{as.graphicsAnnot}
\alias{as.graphicsAnnot}
\title{Coerce an Object for Graphics Annotation}
\description{
  Coerce am \R object into a form suitable for graphics annotation.
}
\usage{
as.graphicsAnnot(x)
}
\arguments{
  \item{x}{An \R object}
}
\details{
  Expressions, calls and names (as used by \link{plotmath}) are passed
  through unchanged.  All other objects with an S3 class (as determined
  by \code{\link{is.object}}) are coerced by \code{\link{as.character}}
  to character vectors.

  All the \pkg{graphics} and \pkg{grid} functions which use this coerce
  calls and names to expressions internally.
}
\value{
  A language object or a character vector.
}
\keyword{utilities}
