% File src/library/tools/man/getDepList.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2008-2021 R Core Team
% Distributed under GPL 2 or later

\name{dependsOnPkgs}
\alias{dependsOnPkgs}
\title{Find Reverse Dependencies}
\description{
  Find \sQuote{reverse} dependencies of packages, that is those packages
  which depend on this one, and (optionally) so on recursively.
}
\usage{
dependsOnPkgs(pkgs,
              dependencies = "strong",
              recursive = TRUE, lib.loc = NULL,
              installed =
              utils::installed.packages(lib.loc, fields = "Enhances"))
}
\arguments{
  \item{pkgs}{a character vector of package names.}
  \item{dependencies}{a character vector listing the types of
    dependencies, a subset of
    \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}.
    Character string \code{"all"} is shorthand for that vector,
    character string \code{"most"} for the same vector without
    \code{"Enhances"},
    character string \code{"strong"} (default) for the first three
    elements of that vector.}
  \item{recursive}{logical: should reverse dependencies of reverse
    dependencies (and so on) be included?}
  \item{lib.loc}{a character vector of \R library trees, or \code{NULL}
    for all known trees (see \code{\link{.libPaths}}).}
  \item{installed}{a result of calling \code{\link{installed.packages}}.}
}
\value{
  A character vector of package names, which does not include any from
  \code{pkgs}.
}
\seealso{
  \code{\link{package_dependencies}()} to get the regular
  (\dQuote{forward}) dependencies of a package.
}
\examples{\donttest{
% do not test as result depends if recommended packages are installed.
## there are few dependencies in a vanilla R installation:
## lattice may not be installed
dependsOnPkgs("lattice")
}}
\keyword{utilities}
