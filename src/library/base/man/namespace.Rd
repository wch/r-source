\name{namespace}
\title{Name Spaces}
\alias{::}
\description{
  Name Space Support Functions.
}
\usage{
pkg::name
}
\arguments{
  \item{pgk}{package name symbol or string literal.}
  \item{name}{variable name symbol or string literal.}
}
\details{
  The expression \code{pkg::name} returns the value of the exported
  variable \code{name} in package \code{pkg}.

  Currently only the base name space is supported.

  Assignment into name spaces is not supported.
}
\examples{
base::log
base::"+"
}
\keyword{programming}
