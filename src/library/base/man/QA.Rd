\name{QA}
\alias{checkDocArgs}
\alias{checkMethods}
\alias{checkTnF}
\title{QA checks for R code and documentation}
\description{
  Functions for performing various quality checks.
}
\usage{
checkDocArgs(dir)
checkMethods(dir)
checkTnF(file, package, lib.loc = .lib.loc)
}
\arguments{
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This should contain the subdirectories \code{R}
    (for R code) and \file{man} with \R documentation sources (in Rd
    format).}
  \item{file}{the name of a file containing R code to be checked.}
  \item{package}{a character string naming an installed package.  If
    given and \code{file} is not given, the installed R code of the
    package is checked.  Currently, there is no support for R code
    installed as an image file.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search for \code{package}.}
}
\details{
  \code{checkDocArgs} checks, for all Rd files in a package, whether all
  arguments shown in the usage sections of the Rd file are documented in
  its arguments section.  It also reports duplicated entries in the
  arguments section.

  \code{checkMethods} checks whether all methods defined in the package
  R code have all arguments of the corresponding generic, with
  positional arguments of the generics in the same positions for the
  method.  The generics are sought first in the given package and then
  in the base package.  The rules when \code{\dots} is involved are
  subtle: see the source code.

  \code{checkTnF} checks the specified R code for occurrences of
  \code{T} or \code{F}, and reports the expression containing these.
  This is useful as in R \code{T} and \code{F} are just variables which
  are set to the logicals \code{TRUE} and \code{FALSE} by default, but
  are not reserved words and hence can be overwritten by the user.
  Hence, one should always use \code{TRUE} and \code{FALSE} for the
  logicals.
}
\section{Warning}{
  These functions are still experimental.  Both names and interfaces
  might change in future versions.
}
\keyword{utilities}
\keyword{documentation}
