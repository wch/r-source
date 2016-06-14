% File src/library/base/man/library.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{library}
\title{Loading/Attaching and Listing of Packages}
\alias{library}
\alias{require}

\alias{.noGenerics}
\alias{format.libraryIQR}
\alias{print.libraryIQR}
\alias{format.packageInfo}
\alias{print.packageInfo}
\description{
  \code{library} and \code{require} load and attach add-on packages.
}
\usage{
library(package, help, pos = 2, lib.loc = NULL,
        character.only = FALSE, logical.return = FALSE,
        warn.conflicts = TRUE, quietly = FALSE,
        verbose = getOption("verbose"))

require(package, lib.loc = NULL, quietly = FALSE,
        warn.conflicts = TRUE,
        character.only = FALSE)
}
\arguments{
  \item{package, help}{the name of a package, given as a \link{name} or
    literal character string, or a character string, depending on
    whether \code{character.only} is \code{FALSE} (default) or
    \code{TRUE}).}
  \item{pos}{the position on the search list at which to attach the
    loaded namespace.  Can also be the name of a position on the current
    search list as given by \code{\link{search}()}.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known to
    \code{\link{.libPaths}()}.
    Non-existent library trees are silently ignored.}
  \item{character.only}{a logical indicating whether \code{package} or
    \code{help} can be assumed to be character strings.}
  \item{logical.return}{logical.  If it is \code{TRUE},  \code{FALSE} or
    \code{TRUE} is returned to indicate success.}
  \item{warn.conflicts}{logical.  If \code{TRUE}, warnings are
    printed about \code{\link{conflicts}} from attaching the new
    package.  A conflict is a function masking a function,
    or a non-function masking a non-function.
  }
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
  \item{quietly}{a logical.  If \code{TRUE}, no message confirming
    package attaching is printed, and most often, no errors/warnings are
    printed if package attaching fails.}
}
\details{
  \code{library(package)} and \code{require(package)} both load the
  namespace of the package with name \code{package} and attach it on the
  search list.  \code{require} is designed for use inside other
  functions; it returns \code{FALSE} and gives a warning (rather than an
  error as \code{library()} does by default) if the package does not
  exist.  Both functions check and update the list of currently attached
  packages and do not reload a namespace which is already loaded.  (If
  you want to reload such a package, call \code{\link{detach}(unload =
  TRUE)} or \code{\link{unloadNamespace}} first.)  If you want to load a
  package without attaching it on the search list, see
  \code{\link{requireNamespace}}.

  To suppress messages during the loading of packages use
  \code{\link{suppressPackageStartupMessages}}: this will suppress all
  messages from \R itself but not necessarily all those from package
  authors.

  If \code{library} is called with no \code{package} or \code{help}
  argument, it lists all available packages in the libraries specified
  by \code{lib.loc}, and returns the corresponding information in an
  object of class \code{"libraryIQR"}.  (The structure of this class may
  change in future versions.)  Use \code{.packages(all = TRUE)} to
  obtain just the names of all available packages, and
  \code{\link{installed.packages}()} for even more information.

  \code{library(help = somename)} computes basic information about the
  package \pkg{somename}, and returns this in an object of class
  \code{"packageInfo"}.  (The structure of this class may change in
  future versions.)  When used with the default value (\code{NULL}) for
  \code{lib.loc}, the attached packages are searched before the libraries.
}
\value{
  Normally \code{library} returns (invisibly) the list of attached
  packages, but \code{TRUE} or \code{FALSE} if \code{logical.return} is
  \code{TRUE}.  When called as \code{library()} it returns an object of
  class \code{"libraryIQR"}, and for \code{library(help=)}, one of
  class \code{"packageInfo"}.

  \code{require} returns (invisibly) a logical indicating whether the required
  package is available.
}
\section{Licenses}{
  Some packages have restrictive licenses, and there is a mechanism to
  allow users to be aware of such licenses.  If
  \code{\link{getOption}("checkPackageLicense") == TRUE}, then at first
  use of a package with a not-known-to-be-FOSS (see below) license the
  user is asked to view and accept the license: a list of accepted
  licenses is stored in file \file{~/.R/licensed}.  In a non-interactive
  session it is an error to use such a package whose license has not
  already been accepted.

  Free or Open Source Software (FOSS,
  e.g.\sspace{}\url{https://en.wikipedia.org/wiki/FOSS}) packages are
  determined by the same filters used by
  \code{\link{available.packages}} but applied to just the current
  package, not its dependencies.

  There can also be a site-wide file \file{R_HOME/etc/licensed.site} of
  packages (one per line).
}
\section{Formal methods}{
  \code{library} takes some further actions when package \pkg{methods}
  is attached (as it is by default).  Packages may define formal generic
  functions as well as re-defining functions in other packages (notably
  \pkg{base}) to be generic, and this information is cached whenever
  such a namespace is loaded after \pkg{methods} and re-defined functions
  (\link{implicit generic}s) are excluded from the list of conflicts.
  The caching and check for conflicts require looking for a pattern of
  objects; the search may be avoided by defining an object
  \code{.noGenerics} (with any value) in the namespace.  Naturally, if the
  package \emph{does} have any such methods, this will prevent them from
  being used.
}
\note{
  \code{library} and \code{require} can only load/attach an
  \emph{installed} package, and this is detected by having a
  \file{DESCRIPTION} file containing a \samp{Built:} field.

  Under Unix-alikes, the code checks that the package was installed
  under a similar operating system as given by \code{R.version$platform}
  (the canonical name of the platform under which R was compiled),
  provided it contains compiled code.  Packages which do not contain
  compiled code can be shared between Unix-alikes, but not to other OSes
  because of potential problems with line endings and OS-specific help
  files.  If sub-architectures are used, the OS similarity is not
  checked since the OS used to build may differ
  (e.g.\sspace{}\code{i386-pc-linux-gnu} code can be built on an
  \code{x86_64-unknown-linux-gnu} OS).

  The package name given to \code{library} and \code{require} must match
  the name given in the package's \file{DESCRIPTION} file exactly, even
  on case-insensitive file systems such as are common on Windows and
  macOS.
}
\seealso{
  \code{\link{.libPaths}}, \code{\link{.packages}}.

  \code{\link{attach}}, \code{\link{detach}}, \code{\link{search}},
  \code{\link{objects}}, \code{\link{autoload}}, 
  \code{\link{requireNamespace}},
  \code{\link{library.dynam}}, \code{\link{data}},
  \code{\link{install.packages}} and
  \code{\link{installed.packages}};
  \code{\link{INSTALL}}, \code{\link{REMOVE}}.
  
  The initial set of packages attached is set by
  \code{\link{options}(defaultPackages=)}: see also \code{\link{Startup}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
library()                   # list all available packages
library(lib.loc = .Library) # list all packages in the default library
\donttest{library(help = splines)     # documentation on package 'splines'}
library(splines)            # attach package 'splines'
require(splines)            # the same
search()                    # "splines", too
detach("package:splines")

# if the package name is in a character vector, use
pkg <- "splines"
library(pkg, character.only = TRUE)
detach(pos = match(paste("package", pkg, sep = ":"), search()))

require(pkg, character.only = TRUE)
detach(pos = match(paste("package", pkg, sep = ":"), search()))

require(nonexistent)        # FALSE
\dontrun{
## if you want to mask as little as possible, use
library(mypkg, pos = "package:base")
}}
\keyword{data}
