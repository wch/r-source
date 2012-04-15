% File src/library/tools/man/checkTnF.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{checkTnF}
\alias{checkTnF}
\alias{print.checkTnF}
\title{Check R Packages or Code for T/F}
\description{
  Checks the specified R package or code file for occurrences of
  \code{T} or \code{F}, and gathers the expression containing these.
  This is useful as in R \code{T} and \code{F} are just variables which
  are set to the logicals \code{TRUE} and \code{FALSE} by default, but
  are not reserved words and hence can be overwritten by the user.
  Hence, one should always use \code{TRUE} and \code{FALSE} for the
  logicals.
}
\usage{
checkTnF(package, dir, file, lib.loc = NULL)
}
\arguments{
  \item{package}{a character string naming an installed package.  If
    given, the installed R code and the examples in the documentation
    files of the package are checked.  R code installed as an image file
    cannot be checked.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This must contain the subdirectory \file{R} (for
    R code), and should also contain \file{man} (for documentation).
    Only used if \code{package} is not given.  If used, the R code files
    and the examples in the documentation files are checked.}
  \item{file}{the name of a file containing R code to be checked.  Used
    if neither \code{package} nor \code{dir} are given.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    search for \code{package}.}
}
\value{
  An object of class \code{"checkTnF"} which is a list containing, for
  each file where occurrences of \code{T} or \code{F} were found, a list
  with the expressions containing these occurrences.  The names of the
  list are the corresponding file names.

  There is a \code{print} method for nicely displaying the information
  contained in such objects.
}
\keyword{utilities}
\keyword{documentation}
