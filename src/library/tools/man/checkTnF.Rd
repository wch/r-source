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
checkTnF(file, package, dir, lib.loc = .lib.loc)
}
\arguments{
  \item{file}{the name of a file containing R code to be checked.}
  \item{package}{a character string naming an installed package.  If
    given and \code{file} is not given, the installed R code of the
    package is checked.  R code installed as an image file cannot be
    checked.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This should contain the subdirectory \code{R}
    (for R code).  Used if neither \code{file} not \code{package} are
    given.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search for \code{package}.}
}
\section{Warning}{
  This function is still experimental.  Both name and interface might
  change in future versions.
}
\keyword{utilities}
\keyword{documentation}
