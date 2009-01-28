\name{testInstalledPackage}
\alias{testInstalledPackage}
\alias{testInstalledPackages}
\title{Test Installed Packages}
\description{
  These functions allow an installed package to be tested, or all base
  and recommended packages.
}
\usage{
testInstalledPackage(pkg, lib.loc = NULL, outDir = ".")

testInstalledPackages(outDir = ".", errorsAreFatal = TRUE)
}
\arguments{
  \item{pkg}{name of an installed package.}
  \item{lib.loc}{library path(s) in which to look for the package.  See
    \code{\link{library}}.}
  \item{outDir}{the directory into which to write the ouptut files.}
  \item{errorsAreFatal}{logical: should testing terminate at the first error?}
}
\details{
  These tests depend on having the package example file installed (which
  is the default).  If package-specific tests are found in a
  \file{tests} directory they too will be tested: these are not
  installed by default, but wlll be if
  \command{R CMD INSTALL --install-tests} was used.

  Package tests are run in the \file{tests} directory of the package,
  and leave output there.  [This may change in future versions.]

  Base and recommended packages are testing in alphabetic order in the
  current locale.

#ifdef unix
  The package-specific tests for the base and recommended packages are
  not normally installed, but \command{make install-tests} is provided
  to do so.
#endif
#ifdef windows
  The package-specific tests for the base and recommended packages are
  an optional part of the install: for testing package \pkg{tools} you
  will also need the help page sources installed (also
  optional). Currently testing requires \command{unzip} in the path, and
  a Unix-like \code{diff} if more than simple differences are found.
#endif
}
\value{
  Invisbly \code{0L} for success, \code{1L} for failure.
}
\keyword{utilities}
