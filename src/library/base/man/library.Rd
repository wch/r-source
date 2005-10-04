\name{library}
\title{Loading and Listing of Packages}
\alias{library}
\alias{require}
\alias{R_LIBS}
\alias{.First.lib}
\alias{.Last.lib}
\alias{.noGenerics}
\alias{print.libraryIQR}
\alias{print.packageInfo}
\description{
  \code{library} and \code{require} load add-on packages.

  \code{.First.lib} is called when a package is loaded;
  \code{.Last.lib} is called when a package is detached.
}
\usage{
library(package, help, pos = 2, lib.loc = NULL,
        character.only = FALSE, logical.return = FALSE,
        warn.conflicts = TRUE,
        keep.source = getOption("keep.source.pkgs"),
        verbose = getOption("verbose"),
        version)

require(package, quietly = FALSE, warn.conflicts = TRUE,
        keep.source = getOption("keep.source.pkgs"),
        character.only = FALSE, version, save = TRUE)

.First.lib(libname, pkgname)
.Last.lib(libpath)
}
\arguments{
  \item{package, help}{the name of a package, given as a \link{name} or
    literal character string, or a character string, depending on
    whether \code{character.only} is \code{FALSE} (default) or
    \code{TRUE}).}
  \item{pos}{the position on the search list at which to attach the
    loaded package.  Note that \code{.First.lib} may attach other
    packages, and \code{pos} is computed \emph{after} \code{.First.lib}
    has been run.  Can also be the name of a position on the current
    search list as given by \code{\link{search}()}.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.
    Non-existent library trees are silently ignored.}
  \item{character.only}{a logical indicating whether \code{package} or
    \code{help} can be assumed to be character strings.}
  \item{version}{A character string denoting a version number of the
     package to be loaded, for use with \emph{versioned installs}: see
     the section later in this document.}
  \item{logical.return}{logical.  If it is \code{TRUE},  \code{FALSE} or
    \code{TRUE} is returned to indicate success.}
  \item{warn.conflicts}{logical.  If \code{TRUE}, warnings are
    printed about \code{\link{conflicts}} from attaching the new
    package, unless that package contains an object \code{.conflicts.OK}.}
  \item{keep.source}{logical.  If \code{TRUE}, functions
    \dQuote{keep their source} including comments, see argument
    \code{keep.source} to \code{\link{options}}.  This applies only to
    the named package, and not to any packages or namespaces which might be
    loaded to satisfy dependencies or imports.

    This argument does not apply to packages using lazy-loading or saved
    images.  Whether they have kept source is determined when they are
    installed (and is most likely false).
  }
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
  \item{quietly}{a logical.  If \code{TRUE}, no message confirming
    package loading is printed.}
  \item{save}{logical or environment.  If \code{TRUE}, a call to
    \code{require} from the source for a package will save the name of
    the required package in the variable \code{".required"}, allowing
    function \code{\link{detach}} to warn if a required package is
    detached. See section \sQuote{Packages that require other packages} below.}
  \item{libname}{a character string giving the library directory where
    the package was found.}
  \item{pkgname}{a character string giving the name of the package,
    including the version number if the package was installed with
    \code{--with-package-versions}.}
  \item{libpath}{a character string giving the complete path to the
    package.}
}
\details{
  \code{library(package)} and \code{require(package)} both load the
  package with name \code{package}.  \code{require} is designed for use
  inside other functions; it returns \code{FALSE} and gives a warning
  (rather than an error as \code{library()} does) if the package does
  not exist.  Both functions check and update the list of currently
  loaded packages and do not reload code that is already loaded.

  If \code{library} is called with no \code{package} or \code{help}
  argument, it lists all available packages in the libraries specified
  by \code{lib.loc}, and returns the corresponding information in an
  object of class \code{"libraryIQR"}.  The structure of this class may
  change in future versions.  In earlier versions of R, only the names
  of all available packages were returned; use \code{.packages(all =
    TRUE)} for obtaining these.  Note that
  \code{\link{installed.packages}()}
  returns even more information.

  \code{library(help = somename)} computes basic information about the
  package \code{somename}, and returns this in an object of class
  \code{"packageInfo"}.  The structure of this class may change in
  future versions.  When used with the default value (\code{NULL}) for
  \code{lib.loc}, the loaded packages are searched before the libraries.

  \code{.First.lib} is called when a package without a namespace is
  loaded by \code{library}.  (For packages with namespaces see
  \code{\link{.onLoad}}.)  It is called with two arguments, the name of
  the library directory where the package was found (i.e., the
  corresponding element of \code{lib.loc}), and the name of the package
  (in that order, and with the package name including the version for a
  versioned install, e.g. \code{tree_1.0-16}).  It is a good place to
  put calls to \code{\link{library.dynam}} which are needed when loading
  a package into this function (don't call \code{library.dynam}
  directly, as this will not work if the package is not installed in a
  \dQuote{standard} location).  \code{.First.lib} is invoked after the
  search path interrogated by \code{search()} has been updated, so
  \code{as.environment(match("package:name", search()))} will return the
  environment in which the package is stored.  If calling
  \code{.First.lib} gives an error the loading of the package is
  abandoned, and the package will be unavailable.  Similarly, if the
  option \code{".First.lib"} has a list element with the package's name,
  this element is called in the same manner as \code{.First.lib} when
  the package is loaded.  This mechanism allows the user to set package
  \dQuote{load hooks} in addition to startup code as provided by the
  package maintainers, but \code{\link{setHook}} is preferred.

  \code{.Last.lib} is called when a package is detached.  Beware
  that it might be called if \code{.First.lib} has failed, so it
  should be written defensively. (It is called within \code{\link{try}},
  so errors will not stop the package being detached.)
}
\value{
  \code{library} returns the list of loaded (or available) packages
  (or \code{TRUE} if \code{logical.return} is \code{TRUE}).
  \code{require} returns a logical indicating whether the required
  package is available.
}
\section{Packages that require other packages}{
  The source code for a
  package that requires one or more other packages should have a call
  to \code{require}, preferably near the beginning of the source, and
  of course before any code that uses functions, classes or methods
  from the other package.  The default for argument \code{save} will
  save the names of all required packages in the environment of the
  new package.  The saved package names are used by
  \code{\link{detach}} when a package is detached to warn if other
  packages still require the detached package.  Also, if a package is
  installed with saved image (see \link{INSTALL}), the saved package
  names are used to require these packages when the new package is
  attached.
}
\section{Formal methods}{
  \code{library} takes some further actions when package \pkg{methods}
  is attached (as it is by default).  Packages may define formal generic
  functions as well as re-defining functions in other packages (notably
  \pkg{base}) to be generic, and this information is cached whenever
  such a package is loaded after \pkg{methods} and re-defined functions
  are excluded from the list of conflicts.  The check requires looking
  for a pattern of objects; the pattern search may be avoided by
  defining an object \code{.noGenerics}
  (with any value) in the package.  Naturally, if the package
  \emph{does} have any such methods, this will prevent them from being used.
}
\section{Versioned installs}{
  Packages can be installed with version information by \code{R CMD
    \link{INSTALL} --with-package-versions} or
  \code{\link{install.packages}(installWithVers = TRUE)}.  This allows
  more than one version of a package to be installed in a library
  directory, using package directory names like \code{foo_1.5-1}.  When
  such packages are loaded, it is this \emph{versioned}  name
  that \code{\link{search}()} returns.  Some utility functions require
  the versioned name and some the unversioned name (here \code{foo}).

  If \code{version} \emph{is not} specified, \code{library} looks first
  for a packages of that name, and then for versioned installs of the
  package, selecting the one with the latest version number.  If
  \code{version} \emph{is} specified, a versioned install with an
  exactly matching version is looked for.

  If \code{version} is not specified, \code{require} will accept any
  version that is already loaded, whereas \code{library} will look for
  an unversioned install even if a versioned install is already loaded.

  Loading more than one version of a package into an \R session is not
  currently supported.  Support for versioned installs is patchy.
}
\note{
  \code{library} and \code{require} can only load an \emph{installed}
  package, and this is detected by having a \file{DESCRIPTION} file
  containing a \code{Built:} field.

  Under Unix-alikes, the code checks that the package was installed
  under a similar operating system as given by \code{R.version$platform}
  (the canonical name of the platform under which R was compiled),
  provided it contains compiled code.  Packages which do not contain
  compiled code can be shared between Unix-alikes, but not to other OSes
  because of potential problems with line endings and OS-specific help
  files.

  As of version 2.0.0, the package name given to \code{library} and
  \code{require} must match the name given in the package's
  \code{DESCRIPTION} file exactly, even on case-insensitive file systems
  such as MS Windows.
}
\seealso{
  \code{\link{.libPaths}}, \code{\link{.packages}}.

  \code{\link{attach}}, \code{\link{detach}}, \code{\link{search}},
  \code{\link{objects}}, \code{\link{autoload}},
  \code{\link{library.dynam}}, \code{\link{data}},
  \code{\link{install.packages}} and
  \code{\link{installed.packages}};
  \code{\link{INSTALL}}, \code{\link{REMOVE}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
library()                   # list all available packages
library(lib = .Library)     # list all packages in the default library
library(help = splines)     # documentation on package 'splines'
library(splines)            # load package 'splines'
require(splines)            # the same
search()                    # "splines", too
detach("package:splines")

# if the package name is in a character vector, use
pkg <- "splines"
library(pkg, character.only = TRUE)
detach(pos = match(paste("package", pkg, sep=":"), search()))

require(pkg, character.only = TRUE)
detach(pos = match(paste("package", pkg, sep=":"), search()))

require(nonexistent)        # FALSE
\dontrun{
## Suppose a package needs to call a shared library named 'fooEXT',
## where 'EXT' is the system-specific extension.  Then you should use
.First.lib <- function(lib, pkg) {
  library.dynam("foo", pkg, lib)
}

## if you want to mask as little as possible, use
library(mypkg, pos = "package:base")
}}
\keyword{data}
