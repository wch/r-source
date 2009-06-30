% File src/library/tools/man/testInstalledPackage.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{testInstalledPackage}
\alias{testInstalledPackage}
\alias{testInstalledPackages}
\alias{testInstalledBasic}
\title{Test Installed Packages}
\description{
  These functions allow an installed package to be tested, or all base
  and recommended packages.
}
\usage{
testInstalledPackage(pkg, lib.loc = NULL, outDir = ".",
                     types = c("examples", "tests", "vignettes"))

testInstalledPackages(outDir = ".", errorsAreFatal = TRUE,
                      scope = c("both", "base", "recommended"),
                      types = c("examples", "tests", "vignettes"))

testInstalledBasic(scope = c("basic", "devel", "both"))
}
\arguments{
  \item{pkg}{name of an installed package.}
  \item{lib.loc}{library path(s) in which to look for the package.  See
    \code{\link{library}}.}
  \item{outDir}{the directory into which to write the output files: this
    should already exist.}
  \item{types}{type(s) of tests to be done.}
  \item{errorsAreFatal}{logical: should testing terminate at the first
    error?}
  \item{scope}{Which set(s) should be tested?}
}
\details{
  These tests depend on having the package example files installed (which
  is the default).  If package-specific tests are found in a
  \file{tests} directory they can be tested: these are not
  installed by default, but will be if
  \command{R CMD INSTALL --install-tests} was used.  Finally, the \R
  code in any vignettes can be extracted and tested.

  Package tests are run in a \file{\var{pkg}-tests} subdirectory of
  \file{outDir}, and leave their output there.

  \code{testInstalledBasic} runs the basic tests, if installed.  This
  should be run with \code{LC_COLLATE=C} set: the function tries to set
  this by it may not work on all OSes.
  
#ifdef unix
  The package-specific tests for the base and recommended packages are
  not normally installed, but \command{make install-tests} is provided
  to do so (as well as the basic tests).
#endif
#ifdef windows
  The package-specific tests for the base and recommended packages are
  an optional part of the install: for testing package \pkg{tools} you
  will also need the help page sources installed (also optional).
  Currently testing requires a Unix-like \code{diff} for a full report
  if more than simple differences are found.
#endif
}
\value{
  Invisibly \code{0L} for success, \code{1L} for failure.
}
\keyword{utilities}
