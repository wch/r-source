\name{base-internal}
\alias{.find.package}
\alias{.path.package}
\alias{.getRequiredPackages}
\alias{.getRequiredPackages2}
\title{Internal Functions in the Base Package}
\description{
  Internal functions in the base package, which are only user-visible
  because of the special nature of the base namespace.
}
\usage{
.find.package(package, lib.loc = NULL, quiet = FALSE,
              verbose = getOption("verbose"))

.path.package(package, quiet = FALSE)

.getRequiredPackages(file = "DESCRIPTION", quietly = FALSE)
.getRequiredPackages2(pkgInfo, quietly = FALSE)
}
\arguments{
  \item{package}{the name of a package}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.}
  \item{quiet}{logical.  Should this not give warnings or an error
    if the package(s) are not found?}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed.}
  \item{file}{The full path to the \file{DESCRIPTION} file of a package.}
  \item{quietly}{logical: should progress messages be given.}
  \item{pkginfo}{A processed \file{DESCRIPTION} file of class
    \code{"packageDescription2"}.}
}
\details{
  \code{.find.package} returns the paths to the locations where the
  given packages can be found.  If \code{lib.loc} is \code{NULL}, then
  then attached packages are searched before the libraries.  If a
  package is found more than once, the first match is used.  Unless
  \code{quiet = TRUE} a warning will be given about the named packages
  which are not found, and an error if none are.  If \code{verbose} is
  true, warnings about packages found more than once are given.

  \code{.path.package} returns the paths from which the named packages
  were loaded, or if none were named, for all currently loaded packages.
  Unless \code{quiet = TRUE} it will warn if some of the packages named
  are not loaded, and given an error if none are.  This function is not
  meant to be called by users, and its interface might change in future
  versions.

  \code{.getRequiredPackages} attaches all the packages mentioned in the
  \code{Depends} field: failure to find a package is an error.
}
\value{
  \code{.find.package} and \code{.path.packages} return a character
  vector of paths to installed packages.
  
  \code{.getRequiredPackages} returns \code{invisible()}.
}
\keyword{internal}
