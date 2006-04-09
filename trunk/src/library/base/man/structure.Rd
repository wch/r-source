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
