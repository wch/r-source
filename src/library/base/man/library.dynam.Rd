% File src/library/base/man/library.dynam.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{library.dynam}
\alias{library.dynam}
\alias{library.dynam.unload}
\alias{.dynLibs}
\title{Loading DLLs from Packages}
\description{
  Load the specified file of compiled code if it has not been loaded
  already, or unloads it.
}
\usage{
library.dynam(chname, package, lib.loc,
              verbose = getOption("verbose"),
              file.ext = .Platform$dynlib.ext, \dots)

library.dynam.unload(chname, libpath,
              verbose = getOption("verbose"),
              file.ext = .Platform$dynlib.ext)

.dynLibs(new)
}
\arguments{
  \item{chname}{a character string naming a DLL (also known as a dynamic
    shared object or library) to load.}
  \item{package}{a character vector with the name of package.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through.}
  \item{libpath}{the path to the loaded package whose DLL is to be unloaded.}
  \item{verbose}{a logical value indicating whether an announcement
    is printed on the console before loading the DLL.  The
    default value is taken from the verbose entry in the system
    \code{\link{options}}.}
  \item{file.ext}{the extension (including \samp{.} if used) to append
    to the file name to specify the library to be loaded.  This defaults
    to the appropriate value for the operating system.}
  \item{\dots}{additional arguments needed by some libraries that
    are passed to the call to \code{\link{dyn.load}} to control
    how the library and its dependencies are loaded.}
  \item{new}{a list of \code{"DLLInfo"} objects corresponding to the
    DLLs loaded by packages.  Can be missing.}
}
\details{
  See \code{\link{dyn.load}} for what sort of objects these functions handle.

  \code{library.dynam} is designed to be used inside a package rather
  than at the command line, and should really only be used inside
  \code{\link{.onLoad}}.  The system-specific extension for DLLs (e.g.,
  \file{.so} or \file{.sl} on Unix-alike systems,
  \file{.dll} on Windows) should not be added.
#ifdef windows

  If \code{\dots} does not include a named argument \code{DLLpath},
  \code{\link{dyn.load}} is called with \code{DLLpath} set to the
  package's \file{libs} directory.  See the \dQuote{Windows} section of
  the help on \code{\link{dyn.load}} for how to control where dependent
  DLLs are found.

  See \code{\link{dyn.load}} for comments about diagnostic messages
  which may be seen on Windows.
#endif

  \code{library.dynam.unload} is designed for use in
  \code{\link{.onUnload}}: it unloads the DLL and updates the value of
  \code{.dynLibs()}

  \code{.dynLibs} is used for getting (with no argument) or setting the
  DLLs which are currently loaded by packages (using \code{library.dynam}).
}
\value{
   If \code{chname} is not specified, \code{library.dynam} returns an
  object of class \code{"\link{DLLInfoList}"} corresponding to the DLLs
  loaded by packages.

  If \code{chname} is specified, an object of class
  \code{"\link{DLLInfo}"} that identifies the DLL and which can be used
  in future calls is returned invisibly.  Note that the class
  \code{"\link{DLLInfo}"} has a method for \code{$} which can be used to
  resolve native symbols within that DLL.

  \code{library.dynam.unload} invisibly returns an object of class
  \code{"\link{DLLInfo}"} identifying the DLL successfully unloaded.

  \code{.dynLibs} returns an object of class \code{"\link{DLLInfoList}"}
  corresponding corresponding to its current value.
}
\section{Warning}{
  Do not use \code{\link{dyn.unload}} on a DLL loaded by
  \code{library.dynam}: use \code{library.dynam.unload} to ensure
  that \code{.dynLibs} gets updated.  Otherwise a subsequent call to
  \code{library.dynam} will be told the object is already loaded.

  Note that whether or not it is possible to unload a DLL and then
  reload a revised version of the same file is OS-dependent: see the
  \sQuote{Value} section of the help for  \code{\link{dyn.unload}}.
}
\seealso{
  \code{\link{getLoadedDLLs}} for information on \code{"DLLInfo"} and
  \code{"DLLInfoList"} objects.

  \code{\link{.onLoad}}, \code{\link{library}},
  \code{\link{dyn.load}}, \code{\link{.packages}},
  \code{\link{.libPaths}}

  \code{\link{SHLIB}} for how to create suitable DLLs.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
## Which DLLs were dynamically loaded by packages?
library.dynam()
}
\keyword{data}
