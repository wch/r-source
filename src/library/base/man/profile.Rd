\name{profile}
\alias{profile}
\title{Generic Function for Profiling Models}
\usage{
profile(fitted, ...)
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
\seealso{\code{\link{profile.nls}} in package \code{nls},
    \code{\link{profile.glm} in package \code{MASS}, \dots}
\keyword{models }
