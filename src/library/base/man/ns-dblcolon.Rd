\name{ns-dblcolon}
\title{Double Colon and Triple Colon Operators}
\alias{::}
\alias{:::}
\description{
  Accessing exported and internal variables in a name space.
}
\usage{
pkg::name
pkg:::name
}
\arguments{
  \item{pkg}{package name symbol or literal character string.}
  \item{name}{variable name symbol or literal character string.}
}
\details{
  The expression \code{pkg::name} returns the value of the exported
  variable \code{name} in package \code{pkg} if the package has a name
  space.  The expression \code{pkg:::name} returns the value of the
  internal variable \code{name} in package \code{pkg} if the package has
  a name space.  The package will be loaded if it was not loaded already
  before the call.  Assignment into name spaces is not supported.

  If the package \code{pkg} does not have a name space but is on the
  search path then \code{pkg::name} returns the value of \code{name} in
  the package environent.
}
\seealso{
  \code{\link{get}} to access an object masked by another of the same name.       }
\examples{
base::log
base::"+"
}
\keyword{programming}
