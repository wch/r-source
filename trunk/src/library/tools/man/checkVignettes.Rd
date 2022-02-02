% File src/library/tools/man/checkVignettes.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{checkVignettes}
\alias{checkVignettes}
\alias{print.checkVignettes}
\title{Check Package Vignettes}
\description{
  Check all vignettes of a package by running
  \code{\link{Sweave}} (or other custom weave function) and/or
  \code{\link{Stangle}} (or other custom tangle function) on them.
  All R source code files found after the tangling step are
  \code{\link{source}}ed to check whether all code can be
  executed without errors.
}
\usage{
checkVignettes(package, dir, lib.loc = NULL,
               tangle = TRUE, weave = TRUE, latex = FALSE,
               workdir = c("tmp", "src", "cur"),
               keepfiles = FALSE)
}
\arguments{
  \item{package}{a character string naming an installed package.
    If given, vignette source files are looked for in subdirectory
    \file{doc}.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.
    If given, vignette source files are looked for in subdirectory
    \file{vignettes}.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    search for \code{package}.}
  \item{tangle}{Perform a tangle and \code{\link{source}} the extracted code?}
  \item{weave}{Perform a weave?}
  \item{latex}{logical: if \code{weave} and \code{latex}
    are \code{TRUE} and there is no \file{Makefile} in the vignettes
    directory, run the intermediate \file{.tex} outputs from weaving
    through \code{\link{texi2pdf}}.}
  \item{workdir}{Directory used as working directory while checking the
    vignettes. If \code{"tmp"} then a temporary directory is created,
    this is the default.  If \code{"src"} then the directory containing
    the vignettes itself is used, if \code{"cur"} then the current
    working directory of \R is used.}
  %% R CMD check makes a copy and uses workdir = "src"
  \item{keepfiles}{Delete files in the temporary directory?  This option is
    ignored when \code{workdir != "tmp"}.}
}
\details{
  This function first uses \code{\link{pkgVignettes}} to find the
  package vignettes, and in particular their vignette engines (see
  \code{\link{vignetteEngine}}).

  If \code{tangle} is true, it then runs \code{\link{Stangle}} (or
  other custom tangle function provided by the engine) to produce (one
  or more) \R code files from each vignette, then \code{\link{source}}s
  each code file in turn.

  If \code{weave} is true, the vignettes are run through
  \code{\link{Sweave}} (or other custom weave function provided by the
  engine).  If \code{latex} is also true and there is no \file{Makefile}
  in the vignettes directory, \code{\link{texi2pdf}} is run on the
  intermediate \file{.tex} files from weaving for those vignettes which
  did not give errors in the previous steps.
}
\value{
  An object of class \code{"checkVignettes"}, which is a list with the
  error messages found during the tangle, source, weave and latex steps.
  There is a \code{print} method for displaying the information
  contained in such objects.
}
\keyword{utilities}
\keyword{documentation}
