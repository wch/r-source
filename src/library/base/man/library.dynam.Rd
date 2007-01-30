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
  \item{new}{a list of \code{"DLLInfo"} objects corresponding to the
    shared libraries loaded by packages.  Can be missing.}
}
\details{
  \code{library.dynam} is designed to be used inside a package rather
  than at the command line, and should really only be used inside
  \code{\link{.First.lib}} or \code{\link{.onLoad}}.  The
  system-specific extension for shared libraries
#ifdef unix
  (e.g., \file{.so} or \file{.sl} on Unix systems)
#endif
#ifdef windows
  (\file{.dll} on Windows)
#endif
  should not be added.  Note that to allow for versioned installs, the
  \code{chname} argument should not be set to the \code{pkgname}
  argument of \code{.First.lib} or \code{.onLoad}.

  \code{library.dynam.unload} is designed for use in
  \code{\link{.Last.lib}} or \code{\link{.onUnload}}: it unloads the
  shared object and updates the value of \code{.dynLibs()}

  \code{.dynLibs} is used for getting (with no argument) or setting the
  shared libraries which are currently loaded by packages (using
  \code{library.dynam}).
}
\value{
  If \code{chname} is not specified, \code{library.dynam} returns an
  object of class \code{"DLLInfoList"} corresponding to the shared
  libraries loaded by packages.

  If \code{chname} is specified, an object of class \code{"DLLInfo"} that
  identifies the DLL and can be used in future calls is returned
  invisibly.  For packages that have name spaces, a list of these objects
  is stored in the name space's environment for use at run-time.

  Note that the class \code{DLLInfo} has an overloaded method for
  \code{$} which can be used to resolve native symbols within that
  DLL.
  
  \code{library.dynam.unload} invisibly returns an object of class
  \code{"DLLInfo"} identifying the DLL successfully unloaded.

  \code{.dynLibs} returns an object of class \code{"DLLInfoList"}
  corresponding corresponding to its current value.
}
\section{Warning}{
  Do not use \code{\link{dyn.unload}} on a shared object loaded by
  \code{library.dynam}: use \code{library.dynam.unload} to ensure
  that \code{.dynLibs} gets updated.  Otherwise a subsequent call to
  \code{library.dynam} will be told the object is already loaded.
}
\seealso{
  \code{\link{getLoadedDLLs}} for information on \code{"DLLInfo"} and
  \code{"DLLInfoList"} objects.
  
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
## Which DLLs were "dynamically loaded" by packages?
library.dynam() 
}
\keyword{data}
