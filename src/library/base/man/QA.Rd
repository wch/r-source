\name{QA}
\alias{checkDocArgs}
\alias{checkTnF}
\title{QA checks for R code and documentation}
\description{
  Functions for performing various quality checks.
}
\usage{
checkDocArgs(dir)
checkTnF(file, package, lib.loc = .lib.loc)
}
\arguments{
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This must contain the subdirectory \file{man}
    with \R documentation sources (in Rd format).}
  \item{file}{the name of a file containing R code to be checked.}
  \item{package}{a character string naming an installed package.  If
    given and \code{file} is not given, the installed R code of the
    package is checked.}
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search for \code{package}.}
}
\details{
  \code{checkDocArgs} checks, for all Rd files in a package, whether all
  arguments shown in the usage sections of the Rd file are documented in
  its arguments section.  It also reports duplicated entries in the
  arguments section.

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
