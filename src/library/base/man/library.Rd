\name{library}
\title{Loading and Listing of Packages}
\usage{
library(package, help = NULL, lib.loc = .lib.loc,
        character.only = FALSE, logical.return = FALSE,
        warn.conflicts = TRUE)
require(package, quietly = FALSE, warn.conflicts = TRUE)
provide(package)

.First.lib(libname, pkgname)

.packages(all.available = FALSE, lib.loc = .lib.loc)
.lib.loc
.Library
.Provided
.Autoloaded
}
\alias{library}
\alias{provide}
\alias{require}
\alias{R_LIBS}
\alias{.Autoloaded}
\alias{.First.lib}
\alias{.Library}
\alias{.Provided}
\alias{.packages}
\alias{.lib.loc}
\arguments{
  \item{package, help}{\code{\link{name}} or character string giving the
    name of a package.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through.}
  \item{character.only}{a logical indicating whether \code{package} or
    \code{help} can be assumed to be character strings.}
  \item{logical.return}{logical.  If it is \code{TRUE},  \code{FALSE} or
      \code{TRUE} is returned to indicate success.}
  \item{warn.conflicts}{logical.  If \code{TRUE}, warnings are
    printed about \code{\link{conflicts}} from attaching the new
    package, unless that package contains an object \code{.conflicts.OK}.}
  \item{quietly}{a logical.  If \code{TRUE}, a warning will not be
    printed if the package cannot be found.}
  \item{libname}{a character string giving the library directory where
      the package was found.}
  \item{pkgname}{a character string giving the name of the package.}
  \item{all.available}{logical; if \code{TRUE} return \code{character}
      vector of all available packages in \code{lib.loc}.}
}
\description{
  \code{library} and \code{require} load add-on packages; \code{provide}
  allows code to register services that packages provide.
  \code{.First.lib()} is called when a package is loaded;
  \code{.packages()} returns information about package availability.
}
\details{
  \code{library(package)} and \code{require(package)} both load the
  package with name \code{package}.  \code{require} is designed for use
  inside other functions; it returns \code{FALSE} and optionally gives a
  warning, rather than giving an error, if the package does not exist.
  Both functions check and update the list of currently loaded packages
  and do not reload code that is already loaded.  \code{require} also
  checks the list \code{.Provided}.

  \code{provide} allows code to register services that it provides.  The
  argument is stored in the list \code{.Provided}.  \code{provide}
  returns \code{FALSE} if the package was already present in
  \code{.Provided} or among the packages in \code{search()}.  The main
  use for \code{provide} is when multiple packages share code.  This is
  most likely when the code implements features present in S(-PLUS) but
  not in R.

  If \code{library} is called with no \code{package} or \code{help}
  argument, it gives a list of all available packages in \code{lib.loc}
  and invisibly returns their names (same as
  \code{.packages(all = TRUE)}).

  \code{library(help = somename)} prints information on the package
  \code{somename}, typically by listing the most important user level
  objects it contains.

  \code{.First.lib()} is called when a package is loaded by
  \code{library()}.  It is called with two arguments, the name of the
  library tree where the package was found (i.e., the corresponding
  element of \code{lib.loc}), and the name of the package (in that
  order).  It is a good place to put calls to \code{library.dynam()}
  which are needed when loading a package into this function (don't call
  \code{library.dynam()} directly, as this will not work if the package
  is not installed in a ``standard'' location).  \code{.First.lib()}
  is invoked after \code{search()} has been updated, so
  \code{pos.to.env(match("package:name"), search())} will return the
  environment in which the package is stored.

  \code{.packages()} returns the ``base names'' of the currently attached
  packages \emph{invisibly} whereas
  \code{.packages(all.available = TRUE)} gives (visibly) \emph{all}
  packages available in the library location path \code{lib.loc}.

  \code{.Autoloaded} contains the ``base names'' of the packages for
  which autoloading has been promised.

  \code{.Library} is a character string giving the location of the
  default library, the ``library'' subdirectory of \code{R_HOME}.
  \code{.lib.loc} is a character vector with the locations of all
  library trees that \R should use.  It is initialized at startup from
  the environment variable \code{R_LIBS} (\code{RLIBS} as used by older
  versions of \R is no longer accepted) (which should be a
#ifdef unix
  colon-separated
#endif
#ifdef windows
  semicolon-separated
#endif
  list of directories at which \R library trees are rooted) followed by
  \code{.Library}.
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
    \code{\link{data}}, \code{\link{install.packages}}.
#ifdef unix
    \code{\link{INSTALL}}, \code{\link{REMOVE}}
#endif
}
\examples{
.packages()                 # maybe just "base"
.packages(all = TRUE)       # return all available as char.vector
library()                   # list all available packages
library(lib = .Library)     # list all packages in the default library
library(help = eda)         # documentation on package "eda"
library(eda)                # load package "eda"
require(eda)                # the same
.packages()                 # "eda", too
require(nonexistent)        # FALSE
## Suppose the a package needs to call a shared library named "foo.EXT",
## where "EXT" is the system-specific extension.  Then you should use
.First.lib <- function(lib, pkg) {
  library.dynam("foo", pkg, lib)
}
}
\keyword{data}
