\name{library.dynam}
\alias{library.dynam}
\alias{.Dyn.libs}
\title{Loading Shared Libraries}
\usage{
library.dynam(chname, package = .packages(), lib.loc = .lib.loc,
              verbose, file.ext, \dots)
}
\arguments{
  \item{chname}{a character string naming a shared library to load.}
  \item{package}{a character vector with the names of packages to search
    through.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through.}
  \item{verbose}{a logical value  indicating whether an announcement
    is printed on the console before loading the shared library.
    The default value is taken from the verbose entry in the system options.}
  \item{file.ext}{the extension to append to the file name to specify
    the library to be loaded. This defaults to the appropriate value
    for the operating system.}
  \item{\dots}{additional arguments needed by some libraries that
    are passed to the call to \code{\link{dyn.load}} to control
    how the library is loaded.}
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
  (\file{.dll} on Windows)
#endif
  should not be added.
}
\value{
  The \code{.Dyn.libs} vector with the names of packages
  which have used \code{library.dynam} in the current \R session.

  It is returned as \code{\link{invisible}}, unless the \code{chname}
  argument is missing.

  Users should never set \code{.Dyn.libs} directly.
}
\seealso{
  \code{\link{.First.lib}}, \code{\link{library}},
  \code{\link{dyn.load}}, \code{\link{.packages}},
  \code{\link{.lib.loc}}

#ifdef unix
  \code{\link{SHLIB}} for how to create suitable shared libraries.
#endif
#ifdef windows
  \code{\link{SHLIB}} for how to create suitable DLLs.
#endif
}
\examples{
library.dynam()# which packages have been ``dynamically loaded''
}
\keyword{data}
