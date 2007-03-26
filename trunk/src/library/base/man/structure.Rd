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
  \code{structure} returns the given object with further 
  \link{attributes} set.
}
\details{
  Adding a  class \code{"factor"} will ensure that numeric codes are
  given integer storage mode.

  For historical reasons (these names are used when deparsing),
  attributes \code{".Dim"}, \code{".Dimnames"}, \code{".Names"},
  \code{".Tsp"} and \code{".Label"} are renamed to \code{"dim"},
  \code{"dimnames"}, \code{"names"}, \code{"tsp"} and \code{"levels"}.

  It is possible to give the same tag more than once, in which case the
  last value assigned wins.  As with other ways of assigning attributes,
  using \code{tag=NULL} removes attribute \code{tag} from \code{.Data} if
  it is present.
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
