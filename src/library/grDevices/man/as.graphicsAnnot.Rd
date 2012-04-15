% File src/library/grDevices/man/as.graphicsAnnot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{as.graphicsAnnot}
\alias{as.graphicsAnnot}
\title{Coerce an Object for Graphics Annotation}
\description{
  Coerce an \R object into a form suitable for graphics annotation.
}
\usage{
as.graphicsAnnot(x)
}
\arguments{
  \item{x}{an \R object}
}
\details{
  Expressions, calls and names (as used by \link{plotmath}) are passed
  through unchanged.  All other objects with an explicit class (as
  determined by \code{\link{is.object}}) are coerced by
  \code{\link{as.character}} to character vectors.

  All the \pkg{graphics} and \pkg{grid} functions which use this coerce
  calls and names to expressions internally.
}
\value{
  A language object or a character vector.
}
\keyword{utilities}
