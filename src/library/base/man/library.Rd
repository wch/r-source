\name{library}
\title{Loading and Listing of Packages}
\usage{
library(name, help = NULL, lib.loc = .lib.loc,
	character.only = FALSE, logical.return = FALSE)
require(name, quietly = FALSE)
provide(name)

.First.lib(libname, pkgname)

.packages()
.lib.loc
.Library
.Provided
.Autoloaded
}
\alias{library}
\alias{provide}
\alias{require}
\alias{RLIBS}
\alias{.Autoloaded}
\alias{.First.lib}
\alias{.Library}
\alias{.Provided}
\alias{.packages}
\alias{.lib.loc}
\arguments{
  \item{name, help}{\code{name} or character string giving the name of a
    package.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through.}
  \item{character.only}{a logical indicating whether \code{name} or
    \code{help} can be assumed to be character strings.}
  \item{quietly}{a logical.  If \code{TRUE}, a warning will not be
    printed if the package cannot be found.}
  \item{libname}{a character string giving the library directory where
    the package was found.}
  \item{pkgname}{a character string giving the name of the package.}
}
\description{
  \code{library(name)} and \code{require(name)} both load the package
  with name \code{name}.  \code{require} is designed for use inside
  other functions; it returns \code{FALSE} and optionally gives a
  warning, rather than giving an error, if the package does not exist.
  Both functions check and update the list of currently loaded packages
  and do not reload code that is already loaded.  \code{require} also
  checks the list \code{.Provided}.

  \code{provide} allows code to register services that it provides.  The
  argument is stored in the list \code{.Provided}.  \code{provide}
  returns \code{FALSE} if the name was already present in
  \code{.Provided} or among the packages in \code{search()}.  The main
  use for \code{provide} is when multiple packages share code.	This is
  most likely when the code implements features present in S(-PLUS) but
  not in R. For example, the spline functions \code{ns}, \code{bs} and
  so on are not included in the \R distribution.  A package containing
  these functions can use \code{provide(splines)} to register this fact.
  Another package that needs the functions can execute
  \code{require(splines)} rather than \code{library(splines)} to load
  the spline package only if their functionality is not already available.

  If \code{library} is called with no argument, it gives a list of all
  available packages.  \code{library(help = name)} prints information on
  the package \code{name}, typically by listing the most important user
  level objects it contains.

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
  packages \emph{invisibly}.

  \code{.Autoloaded} contains the ``base names'' of the packages for
  which autoloading has been promised.

  \code{.Library} is a character string giving the location of the
  default library, the ``library'' subdirectory of \code{RHOME}.
  \code{.lib.loc} is a character vector with the locations of all
  library trees that \R should use.  It is initialized at startup from
  the environment variable \code{RLIBS}, which should be a
  colon-separated list of directories at which \R library trees are
  rooted, and \code{.Library}.
}

\section{Creating Packages}{
  Packages provide a mechanism for loading optional code and attached
  documentation as needed.  The \R distribution provides the example
  packages \code{eda}, \code{mva}, and \code{stepfun}.

  A package consists of a subdirectory containing the files
  \file{DESCRIPTION}, \file{INDEX}, and \file{TITLE}, and the
  subdirectories \file{R}, \file{data}, \file{exec}, \file{man}, and
  \file{src} (some of which can be missing).

  The \file{DESCRIPTION} file contains information about authors,
  version, copyright, etc., and looks like

  Package:\cr
  Version:\cr
  Author:\cr
  Description:\cr
  Depends:\cr
  License:

  Continuation lines (e.g., for descriptions longer than one line) start
  with a whitespace character.  The license field should contain an
  explicit statement or a well-known abbreviation (such as `GPL',
  `LGPL', `BSD' and `Artistic'), maybe followed by a reference to the
  actual license file.  It is very important that this information is
  included---otherwise, it may not even be legally correct for others
  to distribute copies of the package.

  The \file{TITLE} file contains a line giving the name of the package
  and a brief description.  \file{INDEX} contains a line for each
  sufficiently interesting object in the package, giving its name and a
  description (functions such as print methods not usually called
  explicitly might not be included).  Note that you can automatically
  create this file using the \code{Rdindex} program in \file{RHOME/etc},
  provided that Perl is available on your system.

  The \file{R} subdirectory contains \R code files.  The code files to
  be installed must start with a (lower- or uppercase) letter and have
  one of the extensions \file{.R}, \file{.S}, \file{.q}, \file{.r}, or
  \file{.s}.  We recommend using \file{.R}, as this extension seems to
  be not used by any other software.  If necessary, one of these files
  (historically \file{zzz.R}) should use \code{library.dynam()}
  \emph{inside} \code{.First.lib()} to load compiled code.

  The \file{man} subdirectory should contain \R documentation files for
  the objects in the package.  The documentation files to be installed
  must also start with a (lower- or uppercase) letter and have the
  extension \file{.Rd} (the default) or \file{.rd}.

  Source and a Makefile for the compiled code is in \file{src},
  containing C, FORTRAN, or Ratfor source.  The Makefile will be
  passed various machine-dependent compile and link flags, examples of
  which can be seen in the \code{eda} package.

  The \file{data} subdirectory is for additional data files the package
  makes available for loading using \code{data()}.  Note that (at least
  currently) all such files are in fact R code files, and must have the
  extension \file{.R}.

  Finally, \file{exec} could contain additional executables the package
  needs, typically Shell or Perl scripts.  This mechanism is currently
  not used by any package, and still experimental.
}
\value{
  \code{library} returns the list of loaded packages (or \code{TRUE} if
  \code{logical.return} is \code{TRUE}).
  \code{require} returns a logical indicating whether the required
  package is available.
}
\seealso{
  \code{\link{attach}}, \code{\link{detach}}, \code{\link{search}},
  \code{\link{objects}}, \code{\link{autoload}},
  \code{\link{library.dynam}};
  \code{\link{INSTALL}}, \code{\link{REMOVE}}
}
\examples{
( .packages() )	    	    # maybe just "base"
library()		    # list all available packages
library(lib = .Library)	    # list all packages in the default library
library(help = eda)	    # documentation on package "eda"
library(eda)		    # load package "eda"
require(eda)		    # the same
( .packages() )             # "eda", too
require(nonexistent)	    # FALSE
## Suppose the a package needs to call a shared library named "foo.EXT",
## where "EXT" is the system-specific extension.  Then you should use
.First.lib <- function(lib, pkg) {
  library.dynam("foo", pkg, lib)
}
}
\keyword{data}
