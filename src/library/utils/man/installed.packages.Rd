% File src/library/utils/man/installed.packages.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{installed.packages}
\alias{installed.packages}
\title{Find Installed Packages}
\description{
  Find (or retrieve) details of all packages installed in the specified
  libraries.
}
\usage{
installed.packages(lib.loc = NULL, priority = NULL,
                   noCache = FALSE, fields = NULL,
                   subarch = .Platform$r_arch, \dots)
}
\arguments{
  \item{lib.loc}{
    character vector describing the location of \R library trees to
    search through, or \code{NULL} for all known trees
    (see \code{\link{.libPaths}}).
  }
  \item{priority}{
    character vector or \code{NULL} (default).  If non-null, used to
    select packages; \code{"high"} is equivalent to
    \code{c("base", "recommended")}.  To select all packages without an
    assigned priority use \code{priority = "NA"}.
  }
  \item{noCache}{Do not use cached information, nor cache it.}

  \item{fields}{a character vector giving the fields to extract from
    each package's \code{DESCRIPTION} file in addition to the default
    ones, or \code{NULL} (default).  Unavailable fields result in
    \code{NA} values.}

  \item{subarch}{character string or \code{NULL}.  If non-null and
    non-empty, used to select packages which are installed for that
    sub-architecture.}

  \item{\dots}{allows unused arguments to be passed down from other functions.}
}
\details{
  \code{installed.packages} scans the \file{DESCRIPTION} files of each
  package found along \code{lib.loc} and returns a matrix of package
  names, library paths and version numbers.

  The information found is cached (by library) for the \R session and
  specified \code{fields} argument, and updated only if the top-level
  library directory has been altered, for example by installing or
  removing a package.  If the cached information becomes confused, it
  can be avoided by specifying \code{noCache = TRUE}.
}
\value{
  A matrix with one row per package, row names the package names and
  column names (currently)
  \code{"Package"}, \code{"LibPath"},
  \code{"Version"}, \code{"Priority"},
  \code{"Depends"}, \code{"Imports"}, \code{"LinkingTo"},
  \code{"Suggests"}, \code{"Enhances"},
  \code{"OS_type"}, \code{"License"} and
  \code{"Built"} (the \R version the package was built under).
  Additional columns can be specified using the \code{fields}
  argument.
}
\note{
  This needs to read several files per installed package, which will be
  slow on Windows and on some network-mounted file systems.
  
  It will be slow when thousands of packages are installed, so do not
  use it to find out if a named package is installed (use
  \code{\link{find.package}} or \code{\link{system.file}}) nor to find
  out if a package is usable (call \code{\link{requireNamespace}} or
  \code{\link{require}} and check the return value) nor to find details
  of a small number of packages (use \code{\link{packageDescription}}).
}
\seealso{
  \code{\link{update.packages}},
  \code{\link{install.packages}},
  \code{\link{INSTALL}}, \code{\link{REMOVE}}.
}
\examples{
## confine search to .Library for speed
str(ip <- installed.packages(.Library, priority = "high"))
ip[, c(1,3:5)]
plic <- installed.packages(.Library, priority = "high", fields = "License")
## what licenses are there:
table( plic[, "License"] )
}
\keyword{utilities}
