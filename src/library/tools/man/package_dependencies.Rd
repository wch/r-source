% File src/library/tools/man/package_dependencies.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2012-2020 R Core Team
% Distributed under GPL 2 or later

\name{package_dependencies}
\alias{package_dependencies}
\title{Computations on the Dependency Hierarchy of Packages}
\description{
   Find (recursively) dependencies or reverse dependencies of packages.
}
\usage{
package_dependencies(packages = NULL, db = NULL, which = "strong",
	             recursive = FALSE, reverse = FALSE,
                     verbose = getOption("verbose"))
}
\arguments{
  \item{packages}{a character vector of package names.}
  \item{db}{character matrix as from \code{\link{available.packages}()}
    (with the default \code{NULL} the results of this call)
    or data frame variants thereof.  Alternatively, a package database
    like the one available from
    \url{https://cran.r-project.org/web/packages/packages.rds}.
  }
  \item{which}{a character vector listing the types of
    dependencies, a subset of
    \code{c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")}.
    Character string \code{"all"} is shorthand for that vector,
    character string \code{"most"} for the same vector without
    \code{"Enhances"},
    character string \code{"strong"} (default) for the first three
    elements of that vector.
  }
  \item{recursive}{a logical indicating whether (reverse) dependencies
    of (reverse) dependencies (and so on) should be included, or a
    character vector like \code{which} indicating the type of (reverse)
    dependencies to be added recursively.}
  \item{reverse}{logical: if \code{FALSE} (default), regular
    dependencies are calculated, otherwise reverse dependencies.}
  \item{verbose}{logical indicating if output should monitor the package
    search cycles.}
}
\value{
  Named list with one element for each package in argument \code{packages},
  each consists of a character vector naming the (recursive) (reverse) dependencies
  of that package.

  For given packages which are not found in the db, \code{NULL} entries are returned,
  as opposed to \code{character(0)} entries which indicate no dependencies.
}
\seealso{
  \code{\link{dependsOnPkgs}}.
}
\examples{
\donttest{
myPkgs <- c("MASS", "Matrix", "KernSmooth", "class", "cluster", "codetools")
pdb <- available.packages()
system.time(
dep1 <- package_dependencies(myPkgs, db = pdb) # all arguments at default
) # very fast
utils::str(dep1, vec.len=10)

system.time( ## reverse dependencies, recursively --- takes much longer:
deps <- package_dependencies(myPkgs, db = pdb, which = "most",
                             recursive = TRUE, reverse = TRUE)
) # seen ~ 10 seconds

lengths(deps) # 2020-05-03: all are 16053, but codetools with 16057

## install.packages(dependencies = TRUE) installs 'most' dependencies
## and the strong recursive dependencies of these: these dependencies
## can be obtained using 'which = "most"' and 'recursive = "strong"'.
## To illustrate on the the first packages with non-missing Suggests:
packages <- pdb[head(which(!is.na(pdb[, "Suggests"]))), "Package"]
package_dependencies(packages, db = pdb,
                     which = "most", recursive = "strong")
}% dont test
}
\keyword{utilities}
