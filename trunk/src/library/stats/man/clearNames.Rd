\name{clearNames}
\alias{clearNames}
\title{Remove the Names from an Object}
\usage{
clearNames(object)
}
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
lapply( women, mean )               # has a names attribute
clearNames( lapply( women, mean ) ) # removes the names
}
\keyword{list}
