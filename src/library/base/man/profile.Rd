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
  \code{\link[nls]{profile.nls}} in package \bold{nls},
  \code{\link[MASS]{profile.glm}} in package \bold{MASS},
  \ldots
}
\keyword{models}
