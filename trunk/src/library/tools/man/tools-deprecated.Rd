% File src/library/tools/man/tools-deprecated.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016, 2018 R Core Team
% Distributed under GPL 2 or later

\name{tools-deprecated}
\title{Deprecated Objects in Package \pkg{tools}}
%------ NOTE:  ../R/tools-deprecated.R   must be synchronized with this!
\alias{tools-deprecated}
%------ PLEASE: one \alias{.} for EACH ! (+ one \usage{} & \arguments{} for all)
\alias{package.dependencies}
\alias{getDepList}
\alias{pkgDepends}
\alias{installFoundDepends}
\alias{vignetteDepends}
\description{
  The functions or variables listed here are provided for compatibility
  with older versions of \R only, and may be defunct as soon as of the
  next release.
}
\usage{
package.dependencies(x, check = FALSE,
                     depLevel = c("Depends", "Imports", "Suggests"))

getDepList(depMtrx, instPkgs, recursive = TRUE, local = TRUE,
           reduce = TRUE, lib.loc = NULL)

pkgDepends(pkg, recursive = TRUE, local = TRUE, reduce = TRUE,
           lib.loc = NULL)

installFoundDepends(depPkgList, ...)

vignetteDepends(vignette, recursive = TRUE, reduce = TRUE,
                local = TRUE, lib.loc = NULL)
}
\arguments{
  \item{x}{A matrix of package descriptions as returned by
    \code{\link{available.packages}}.}
  \item{check}{If \code{TRUE}, return logical vector of check results.
    If \code{FALSE}, return parsed list of dependencies.}
  \item{depLevel}{Whether to look for \code{Depends} or \code{Suggests}
    level dependencies.  Can be abbreviated.}

  \item{depMtrx}{a dependency matrix as from \code{\link{package.dependencies}()}.}
  \item{pkg}{the name of the package}
  \item{instPkgs}{a matrix specifying all packages installed on the
    local system, as from \code{installed.packages}}
  \item{recursive}{whether or not to include indirect dependencies.}
  \item{local}{whether or not to search only locally}
  \item{reduce}{whether or not to collapse all sets of dependencies to a
    minimal value}
  \item{lib.loc}{what libraries to use when looking for installed
    packages.  \code{NULL} indicates all library directories in the
    current \code{.libPaths()}.
    Note that \code{lib.loc} is not used in \code{getDepList()} and
    deprecated there.}

  \item{depPkgList}{A \code{Found} element from a \code{pkgDependsList} object}
  \item{\dots}{Arguments to pass on to \code{\link{install.packages}}}

  %% for vignetteDepends() (if not above):
  \item{vignette}{the path to the vignette source}
}
\seealso{
  \code{\link{Deprecated}},
  \code{\link{Defunct}}
}
\keyword{misc}
