\name{attributes}
\title{Object Attribute Lists}
\usage{
attributes(obj)
attributes(obj) <- list
mostattributes(obj) <- list
}
\alias{attributes}
\alias{attributes<-}
\alias{mostattributes<-}
\description{
  These functions access an object's attribute list.
  The first form above returns the an object's attribute list.
  The assignment forms make the list on the right-hand
  side of the assignment the object's attribute list (if appropriate).
}
\arguments{
  \item{obj}{an object}
}
\details{
  The \code{mostattributes} assignment takes special care for the
  \code{\link{dim}}, \code{\link{names}} and \code{\link{dimnames}}
  attributes, and assigns them only when that is valid whereas as
  \code{attributes} assignment would give an error in that case.
}
\seealso{\code{\link{attr}}.
}
\examples{
x <- cbind(a=1:3, pi=pi) # simple matrix w/ dimnames
str(attributes(x))

## strip an object's attributes:
attributes(x) <- NULL
x # now just a vector of length 6

mostattributes(x) <- list(mycomment = "really special", dim = 3:2,
   dimnames = list(LETTERS[1:3], letters[1:5]), names = paste(1:6))
x # dim(), but not {dim}names
}
\keyword{attribute}
