\name{checkVignettes}
\alias{checkVignettes}
\alias{print.checkVignettes}
\title{Check Package Vignettes}
\description{
  Check all \code{\link{Sweave}} files of a package by running
  \code{\link{Sweave}} and/or \code{\link{Stangle}} on them. All
  R source code files found after the tangling step are
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
 \item{package}{a character string naming an installed package.  If
    given, Sweave files are searched in subdirectory \code{doc}.}
  \item{dir}{a character string specifying the path to a package's root
    source directory.  This subdirectory \code{inst/doc} is searched for
    Sweave files.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  The specified library trees are used to
    to search for \code{package}.}
  \item{tangle}{Perform a tangle and \code{\link{source}} the extracted code?}
  \item{weave}{Perform a weave?}
  \item{latex}{logical: if \code{tangle = weave = latex = TRUE} and there
    is no \code{Makefile} in the vignettes directory, run the tangled
    files through \code{pdflatex}.}
  \item{workdir}{Directory used as working directory while checking the
    vignettes. If \code{"tmp"} then a temporary directory is created,
    this is the default.  If \code{"src"} then the directory containing
    the vignettes itself is used, if \code{"cur"} then the current
    working directory of \R is used.}
  \item{keepfiles}{Delete file in temporary directory? This option is
    ignored when \code{workdir!="tmp"}.}
}
\value{
  An object of class \code{"checkVignettes"} which is a list with the
  error messages found during the tangle and weave steps. There is a
  \code{print} method for nicely displaying the information 
  contained in such objects.
}
\keyword{utilities}
\keyword{documentation}
