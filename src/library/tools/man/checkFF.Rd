% File src/library/tools/man/checkFF.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{checkFF}
\alias{checkFF}
\alias{print.checkFF}
\title{Check Foreign Function Calls}
\usage{
checkFF(package, dir, file, lib.loc = NULL,
        registration = FALSE, check_DUP = FALSE,
        verbose = getOption("verbose"))
}
\description{
  Performs checks on calls to compiled code from R code.  Currently only
  checks whether the interface functions such as \code{.C} and
  \code{.Fortran} are called with a \code{"\link{NativeSymbolInfo}"}
  first argument or with argument \code{PACKAGE} specified, which is
  highly recommended to avoid name clashes in foreign function calls.
}
\arguments{
  \item{package}{a character string naming an installed package.  If
    given, the installed R code of the package is checked.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This should contain the subdirectory \file{R}
    (for R code).  Only used if \code{package} is not given.}
  \item{file}{the name of a file containing R code to be checked.  Used
    if neither \code{package} nor \code{dir} are given.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    search for \code{package}.}
  \item{registration}{a logical.  If \code{TRUE}, checks
    the registration information on the call (if available).}
  \item{check_DUP}{a logical.  If \code{TRUE}, \code{.C} and
    \code{.Fortran} calls with \code{DUP = FALSE} are reported.}
  \item{verbose}{a logical.  If \code{TRUE}, additional diagnostics are
    printed (and the result is returned invisibly).}
}
\details{
  Note that we can only check if the \code{name} argument is a symbol or
  a character string, not what class of object the symbol resolves to
  at run-time.

  If the package has a namespace which contains a \code{useDynLib}
  directive, calls in top-level functions in the package are not
  reported as their symbols will be preferentially looked up in the DLL
  named in the first \code{useDynLib} directive.

  This checks that calls with \code{PACKAGE} specified are to the
  same package, and reports separately those which are in base packages
  and those which are in other packages (and if those packages are
  specified in the \file{DESCRIPTION} file).
}
\value{
  An object of class \code{"checkFF"}.

  There are \code{\link{format}} and \code{print} methods to display the
  information contained in such objects.
}
\seealso{
  \code{\link{.C}},
  \code{\link{.Fortran}};
  \code{\link{Foreign}}.
}
\examples{\donttest{ # order is pretty much random
checkFF(package = "stats", verbose = TRUE)
}}
\keyword{programming}
\keyword{utilities}
