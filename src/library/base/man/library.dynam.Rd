\name{library.dynam}
\alias{library.dynam}
\alias{.Dyn.libs}
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
  Load the specified file of compiled code if it has not been loaded already.
}
\details{
  This is designed to be used inside a
  package rather than at the command line, and should really only be
  used inside \code{\link{.First.lib}()}.  The system-specific extension
  for shared libraries
#ifdef unix
  (e.g., \file{.so} or \file{.sl} on Unix systems)
#endif
#ifdef windows
  (\file{.dd} on Windows)
#endif
  should not be added.
}
\value{
  The \code{.Dyn.libs} vector with the names of packages
  which have used \code{library.dynam(..)} in the current \R session.

  It is returned as \code{\link{invisible}}, unless the \code{chname}
  argument is missing.

  Users should never set \code{.Dyn.libs} directly.
}
\seealso{
  \code{\link{.First.lib}}, \code{\link{library}},
  \code{\link{dyn.load}}, \code{\link{.packages}},
  \code{\link{.lib.loc}}
}
\examples{
library.dynam()# which packages have been ``dynamically loaded''
}
\keyword{data}
