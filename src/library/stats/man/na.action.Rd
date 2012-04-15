% File src/library/stats/man/na.action.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{na.action}
\title{NA Action}
\description{
  Extract information on the NA action used to create an object.
}
\usage{
na.action(object, \dots)
}
\alias{na.action}
\alias{na.action.default}
\arguments{
  \item{object}{any object whose \code{\link{NA}} action is given.}
  \item{\dots}{further arguments special methods could require.}
}
\details{
  \code{na.action} is a generic function, and \code{na.action.default} its
  default method.  The latter extracts the \code{"na.action"} component
  of a list if present, otherwise the \code{"na.action"} attribute.

  When \code{\link{model.frame}} is called, it records any information
  on \code{NA} handling in a \code{"na.action"} attribute.  Most
  model-fitting functions return this as a component of their result.
}
\value{
  Information from the action which was applied to \code{object} if
  \code{NA}s were handled specially, or \code{NULL}.
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S.}
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{options}("na.action")}, \code{\link{na.omit}},
  \code{\link{na.fail}}, also for \code{na.exclude}, \code{na.pass}.
}
\examples{
na.action(na.omit(c(1, NA)))
}
\keyword{NA}
\keyword{methods}
