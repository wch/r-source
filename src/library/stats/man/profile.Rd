\name{profile}
\alias{profile}
\title{Generic Function for Profiling Models}
\usage{
profile(fitted, \dots)
}
\arguments{
  \item{fitted}{the original fitted model object.}
  \item{\dots}{additional parameters. See documentation on individual
    methods.}
}
\description{
  Investigates behavior of objective function near the solution
  represented by \code{fitted}.

  See documentation on method functions for further details.
}
\value{
  A list with an element for each parameter being profiled. See the
  individual methods for further details.
}
\seealso{
  \code{\link{profile.nls}},
  \code{\link[MASS:confint]{profile.glm}} in package \pkg{MASS},
  \ldots

  For profiling code, see \code{\link{Rprof}}.
}
\keyword{models}
