\name{ns-dblcolon}
\title{Double Colon Operator}
\alias{::}
\description{
  Accessing exported variables in a name space.
}
\usage{
pkg::name
}
\arguments{
  \item{pkg}{package name symbol or string literal.}
  \item{name}{variable name symbol or string literal.}
}
\details{
  The expression \code{pkg::name} returns the value of the exported
  variable \code{name} in package \code{pkg} if the package has a name
  space.  The package will be loaded if it was not loaded already
  before the call.  Assignment into name spaces is not supported.
}
\examples{
base::log
base::"+"
}
\keyword{programming}
