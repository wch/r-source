\name{namespace}
\title{Name Spaces}
\alias{::}
\alias{topenv}
\description{
  Name Space Support Functions.
}
\usage{
pkg::name
topenv(envir = parent.frame())
}
\arguments{
  \item{pgk}{package name symbol or string literal.}
  \item{name}{variable name symbol or string literal.}
  \item{envir}{environment.}
}
\details{
  The expression \code{pkg::name} returns the value of the exported
  variable \code{name} in package \code{pkg}.  Currently only the base
  name space is supported.  Assignment into name spaces is not supported.

  \code{topenv} returns the first top level environment found when
  searching \code{envir} and its parent environments.  An environment is
  considered top level if it is the internal environment of a name space,
  a package envirnment in the search path, or \code{.GlobalEnv}.
}
\examples{
base::log
base::"+"
topenv(.GlobalEnv)
topenv(new.env())
}
\keyword{programming}
