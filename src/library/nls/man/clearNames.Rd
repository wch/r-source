\name{clearNames}
\alias{clearNames}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{remove the names from an object}
\usage{
clearNames(object)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{object}{ an object that may have a \code{names} attribute }
}
\description{
 This function sets the \code{names} attribute of \code{object} to
 \code{NULL} and returns the object.
}
\value{
  An object similar to \code{object} but without names.
}
\author{ Douglas Bates and Saikat DebRoy}
\seealso{\code{\link{setNames}}}
\examples{
library( nls )
data( women )
lapply( women, mean )               # has a names attribute
clearNames( lapply( women, mean ) ) # removes the names
}
\keyword{ data, list }%-- one or more ...
