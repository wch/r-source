\name{ns-topenv}
\title{Top Level Environment}
\alias{topenv}
\description{
  Finding the top level environment.
}
\usage{
topenv(envir = parent.frame())
}
\arguments{
  \item{envir}{environment.}
}
\details{
  \code{topenv} returns the first top level environment found when
  searching \code{envir} and its parent environments.  An environment is
  considered top level if it is the internal environment of a name space,
  a package environment in the search path, or \code{.GlobalEnv}.
}
\examples{
topenv(.GlobalEnv)
topenv(new.env())
}
\keyword{programming}
