\name{library.dynam}
\alias{library.dynam}
\alias{library.dynam.unload}
\alias{.dynLibs}
\title{Loading Shared Libraries}
\description{
  Load the specified file of compiled code if it has not been loaded
  already, or unloads it.
}
\usage{
library.dynam(chname, package = NULL, lib.loc = NULL,
              verbose = getOption("verbose"),
              file.ext = .Platform$dynlib.ext, \dots)
library.dynam.unload(chname, libpath,
              verbose = getOption("verbose"),
              file.ext = .Platform$dynlib.ext)
.dynLibs(new)
}
\arguments{
  \item{chname}{a character string naming a shared library to load.}
  \item{package}{a character vector with the names of packages to search
    through, or \code{NULL}.  By default, all packages in the search
    path are used.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.}
  \item{libpath}{the path to the loaded package whose shared library is
    to be unloaded.}
  \item{verbose}{a logical value indicating whether an announcement
    is printed on the console before loading the shared library.  The
    default value is taken from the verbose entry in the system
    options.}
  \item{file.ext}{the extension to append to the file name to specify
    the library to be loaded. This defaults to the appropriate value
    for the operating system.}
  \item{\dots}{additional arguments needed by some libraries that
    are passed to the call to \code{\link{dyn.load}} to control
    how the library is loaded.}
  \item{new}{a character vector of packages which have loaded shared
    libraries.}
}
\details{
  \code{library.dynam} is designed to be used inside a package rather
  than at the command line, and should really only be used inside
  \code{\link{.First.lib}} on \code{\link{.onLoad}}.  The
  system-specific extension for shared libraries
#ifdef unix
  (e.g., \file{.so} or \file{.sl} on Unix systems)
#endif
#ifdef windows
  (\file{.dll} on Windows)
#endif
  should not be added.

  \code{library.dynam.unload} is designed for use in \code{\link{.Last.lib}} or
  \code{\link{.onUnload}}.

  \code{.dynLibs} is used for getting or setting the packages that have
  loaded shared libraries (using \code{library.dynam}).  Versions of \R
  prior to 1.6.0 used an internal global variable \code{.Dyn.libs} for
  storing this information: this variable is now defunct.
}
\value{
  \code{library.dynam} returns a character vector with the names of
  packages which have used it in the current \R session to load shared
  libraries.  This vector is returned as \code{\link{invisible}}, unless
  the \code{chname} argument is missing.

  \code{library.dynam.unload} returns the updated character vector, invisibly.
}
\seealso{
  \code{\link{.First.lib}}, \code{\link{library}},
  \code{\link{dyn.load}}, \code{\link{.packages}},
  \code{\link{.libPaths}}

#ifdef unix
  \code{\link{SHLIB}} for how to create suitable shared libraries.
#endif
#ifdef windows
  \code{\link{SHLIB}} for how to create suitable DLLs.
#endif
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
library.dynam() # which packages have been "dynamically loaded"
}
\keyword{data}
