\name{library.dynam}
\alias{library.dynam}
\title{Loading Shared Libraries}
\usage{
library.dynam(chname, package = .packages(), lib.loc = .lib.loc)
}
\arguments{
  \item{chname}{a character string naming a shared library to load.}
  \item{package}{a character vector with the names of packages to search
    through.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through.}
}
\description{
  \code{library.dynam} loads the specified (shared) object file if it
  has not been loaded already.	It is designed to be used inside a
  package rather than at the command line, and should really only be
  used inside .First.lib().  The system-specific extension for shared
  libraries (e.g., ``.so'' on Unix systems) should not be added.
}
\seealso{
  \code{\link{.First.lib}}, \code{\link{library}},
  \code{\link{dynload}}, \code{\link{.packages}},
  \code{\link{.lib.loc}}
}
\keyword{data}
