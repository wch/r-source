\name{checkTnF}
\alias{checkTnF}
\title{Check R Code for T/F}
\description{
  Checks the specified R code for occurrences of \code{T} or \code{F},
  and reports the expression containing these.  This is useful as in R
  \code{T} and \code{F} are just variables which are set to the logicals
  \code{TRUE} and \code{FALSE} by default, but are not reserved words
  and hence can be overwritten by the user.  Hence, one should always
  use \code{TRUE} and \code{FALSE} for the logicals.
}
\usage{
checkTnF(package, dir, file, lib.loc = NULL)
}
\arguments{
  \item{package}{a character string naming an installed package.  If
    given, the installed R code of the package is checked.  R code
    installed as an image file cannot be checked.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This should contain the subdirectory \code{R}
    (for R code).  Only used if \code{package} is not given.}
  \item{file}{the name of a file containing R code to be checked.  Used
    if neither \code{package} nor \code{dir} are given.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    to search for \code{package}.}
}
\section{Warning}{
  This function is still experimental.  Both name and interface might
  change in future versions.
}
\keyword{utilities}
\keyword{documentation}
