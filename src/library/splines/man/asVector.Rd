\name{asVector}
\alias{asVector.xyVector}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Coerce an object to a vector}
\description{
This is a generic function.  Methods for this function coerce objects of
given classes to vectors.
}
\usage{
asVector(object)
}
\arguments{
    \item{object}{An object.}
}
\value{
    a vector
}
\description{
    Methods for vector coersion in new classes must be created for the
    \code{asVector} generic instead of \code{as.vector}.
    The \code{as.vector} function is internal and not
    easily extended.  Currently the only class with an \code{asVector}
    method is the \code{xyVector} class.
}
\author{Douglas Bates and William Venables}
\seealso{\code{\link{xyVector}}}
\examples{
library( splines )
data( women )
ispl <- interpSpline( weight ~ height,  women )
pred <- predict(ispl)
class(pred)
str(pred)
asVector(pred)
}
\keyword{models}%-- one or more ...
