\name{library}
\title{Loading and Listing of Packages}
\alias{library}
\alias{require}
\alias{R_LIBS}
\alias{.Autoloaded}
\alias{.First.lib}
\alias{.Last.lib}
\alias{.Library}
\alias{.find.package}
\alias{.packages}
\alias{.path.package}
\alias{.lib.loc}
\alias{.libPaths}
\alias{print.libraryIQR}
\description{
  \code{library} and \code{require} load add-on packages.
  \code{.First.lib} is called when a package is loaded;
  \code{.Last.lib} is called when a package is detached.
  \code{.packages} returns information about package availability.
  \code{.path.package} returns information about where a package was
  loaded from.
  \code{.find.package} returns the directory paths of installed
  packages.
}
\usage{
library(package, help, lib.loc = NULL, character.only = FALSE,
        logical.return = FALSE, warn.conflicts = TRUE,
        keep.source = getOption("keep.source.pkgs"),
        verbose = getOption("verbose"))
require(package, quietly = FALSE, warn.conflicts = TRUE,
        keep.source = getOption("keep.source.pkgs"))

.First.lib(libname, pkgname)
.Last.lib(libpath)

.packages(all.available = FALSE, lib.loc = NULL)
.path.package(package = .packages(), quiet = FALSE)
.find.package(package, lib.loc = NULL, use.attached, quiet = FALSE,
              verbose = getOption("verbose"))
.libPaths(new)

.Library
.Autoloaded
}
\arguments{
  \item{package, help}{\link{name} or character string giving the name
    of a package.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.}
  \item{character.only}{a logical indicating whether \code{package} or
    \code{help} can be assumed to be character strings.}
  \item{logical.return}{logical.  If it is \code{TRUE},  \code{FALSE} or
      \code{TRUE} is returned to indicate success.}
  \item{warn.conflicts}{logical.  If \code{TRUE}, warnings are
    printed about \code{\link{conflicts}} from attaching the new
    package, unless that package contains an object \code{.conflicts.OK}.}
  \item{keep.source}{logical.  If \code{TRUE}, functions ``keep their
    source'' including comments, see argument \code{keep.source} to
    \code{\link{options}}.}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}  
  \item{quietly}{a logical.  If \code{TRUE}, a warning will not be
    printed if the package cannot be found.}
  \item{libname}{a character string giving the library directory where
    the package was found.}
  \item{pkgname}{a character string giving the name of the package.}
  \item{libpath}{a character string giving the complete path to the
    package.}
  \item{all.available}{logical; if \code{TRUE} return a character vector
    of all available packages in \code{lib.loc}.}
  \item{quiet}{logical.
    For \code{.path.package}, should this not give warnings or an error
    if the package(s) are not loaded?
    For \code{.find.package}, should this not give warnings or an error
    if the package(s) are not found?}
  \item{use.attached}{a logical indicating whether attached packages
    should be considered in addition to the ones installed in the given
    libraries.}
  \item{new}{a character vector with the locations of \R library trees.}
}
\details{
  \code{library(package)} and \code{require(package)} both load the
  package with name \code{package}.  \code{require} is designed for use
  inside other functions; it returns \code{FALSE} and optionally gives a
  warning, rather than giving an error, if the package does not exist.
  Both functions check and update the list of currently loaded packages
  and do not reload code that is already loaded.

  For large packages, setting \code{keep.source = FALSE} may save quite
  a bit of memory.

  If \code{library} is called with no \code{package} or \code{help}
  argument, it lists all available packages in the libraries specified
  by \code{lib.loc}, and returns the corresponding information in an
  object of class \code{"libraryIQR"}.  The structure of this class may
  change in future versions.  In earlier versions of R, only the names
  of all available packages were returned; use \code{.packages(all =
    TRUE)} for obtaining these.

  \code{library(help = somename)} prints information on the package
  \code{somename}, typically by listing the most important user level
  objects it contains.

  \code{.First.lib} is called when a package is loaded by
  \code{library}.  It is called with two arguments, the name of the
  library directory where the package was found (i.e., the corresponding
  element of \code{lib.loc}), and the name of the package (in that
  order).  It is a good place to put calls to \code{\link{library.dynam}}
  which are needed when loading a package into this function (don't call
  \code{library.dynam} directly, as this will not work if the package
  is not installed in a ``standard'' location).  \code{.First.lib}
  is invoked after the search path interrogated by \code{search()}
  has been updated, so
  \code{as.environment(match("package:name"), search())} will return the
  environment in which the package is stored.  If calling
  \code{.First.lib} gives an error the loading of the package is
  abandoned, and the package will be unavailable.  Similarly, if the
  option \code{".First.lib"} has a list element with the package's name,
  this element is called in the same manner as \code{.First.lib} when
  the package is loaded.  This mechanism allows the user to set package
  ``load hooks'' in addition to startup code as provided by the package
  maintainers.

  \code{.Last.lib} is called when a package is detached.  Beware
  that it might be called if \code{.First.lib} has failed, so it
  should be written defensively. (It is called within \code{\link{try}},
  so errors will not stop the package being detached.)

  \code{.packages()} returns the ``base names'' of the currently attached
  packages \emph{invisibly} whereas
  \code{.packages(all.available = TRUE)} gives (visibly) \emph{all}
  packages available in the library location path \code{lib.loc}.

  \code{.path.package} returns the paths from which the named packages
  were loaded, or if none were named, for all currently loaded packages.
  Unless \code{quiet = TRUE} it will warn if some of the packages named
  are not loaded, and given an error if none are.  This function is not
  meant to be called by users, and its interface might change in future
  versions.

  \code{.find.package} returns the paths to the locations where the
  given packages can be found.  If \code{lib.loc} is \code{NULL}, then
  then attached packages are searched before the libraries.  If a
  package is found more than once, the first match is used.  Unless
  \code{quiet = TRUE} a warning will be given about the named packages
  which are not found, and an error if none are.  If \code{verbose} is
  true, warnings about packages found more than once are given.
  Argument \code{use.attached} is deprecated.  This function is not
  meant to be called by users, and its interface might change in future
  versions.

  \code{.Autoloaded} contains the ``base names'' of the packages for
  which autoloading has been promised.

  \code{.Library} is a character string giving the location of the
  default library, the \file{library} subdirectory of \code{R_HOME}.
  \code{.libPaths} is used for getting or setting the library trees that
  \R knows about (and hence uses when looking for packages).  If called
  with argument \code{new}, the library search path is set to
  \code{unique(new, .Library)} and this is returned.  If given no
  argument, a character vector with the currently known library trees is
  returned.  The library search path is initialized at startup from the
  environment variable \code{R_LIBS} (which should be a
#ifdef unix
  colon-separated
#endif
#ifdef windows
  semicolon-separated
#endif
  list of directories at which \R library trees are rooted) by calling
  \code{.libPaths} with the directories specified in \code{R_LIBS}. 
  Currently, the global variable \code{.lib.loc} is used for storing the
  paths to the known library trees; however, this variable should not be
  accessed directly.
}
\value{
  \code{library} returns the list of loaded (or available) packages
  (or \code{TRUE} if \code{logical.return} is \code{TRUE}).
  \code{require} returns a logical indicating whether the required
  package is available.
}
\author{R core;  Guido Masarotto for the \code{all.available=TRUE}
  part of \code{.packages}.}
\seealso{
  \code{\link{attach}}, \code{\link{detach}}, \code{\link{search}},
  \code{\link{objects}}, \code{\link{autoload}},
  \code{\link{library.dynam}},
  \code{\link{data}}, \code{\link{install.packages}},
  \code{\link{INSTALL}}, \code{\link{REMOVE}}.
}
\examples{
(.packages())		    # maybe just "base"
.packages(all = TRUE)       # return all available as character vector
library()                   # list all available packages
library(lib = .Library)     # list all packages in the default library
library(help = eda)         # documentation on package `eda'
library(eda)                # load package `eda'
require(eda)                # the same
(.packages())		    # "eda", too
detach("package:eda")

# if the package name is in a character vector, use
pkg <- "eda"
library(pkg, character.only = TRUE)
detach(pos = match(paste("package", pkg, sep=":"), search()))

.path.package()
.Autoloaded                 # maybe "ctest"

.libPaths()                 # all library trees R knows about

require(nonexistent)        # FALSE
\dontrun{## Suppose a package needs to call a shared library named `fooEXT',
## where `EXT' is the system-specific extension.  Then you should use
.First.lib <- function(lib, pkg) {
  library.dynam("foo", pkg, lib)
}}
}
\keyword{data}
