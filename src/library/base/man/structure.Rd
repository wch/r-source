\name{structure}
\title{Attribute Specification}
\usage{
structure(.Data, \dots)
}
\alias{structure}
\arguments{
  \item{.Data}{an object which will have
    various attributes attached to it.}
  \item{\dots}{attributes, specified in \code{tag=value}
    form, which will be attached to data.}
}
\description{
  \code{structure} returns the given object with its attributes set.
}
\details{
  Adding a \code{\link{tsp}} attribute will ensure that the object
  returned inherits from class \code{"ts"}, and adding a
  \code{\link{levels}} attribute ensures that the returned object is a
  factor.

  For historical reasons, attributes \code{".Dim"}, \code{".Dimnames"},
  \code{".Names"}, \code{".Tsp"} and \code{".Label"} are renammed to
  \code{"dim"}, \code{"dimnames"}, \code{"names"}, \code{"tsp"} and
  \code{"levels"}.
}
\seealso{
  \code{\link{attributes}}, \code{\link{attr}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
structure(1:6, dim = 2:3)
}
\keyword{attribute}
\keyword{manip}
