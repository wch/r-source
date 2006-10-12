\name{buildVignettes}
\alias{buildVignettes}
\alias{pkgVignettes}
\title{List and Build Package Vignettes}
\description{
  Run \code{\link{Sweave}} and \code{\link{texi2dvi}} on all vignettes
  of a package.
}
\usage{
buildVignettes(package, dir, lib.loc = NULL, quiet = TRUE)
pkgVignettes(package, dir, lib.loc = NULL)
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
  \item{quiet}{logical. Run \code{\link[utils]{Sweave}} and
    \code{\link{texi2dvi}} in quiet mode.}
}
\value{
  \code{buildVignettes} is called for its side effect of creating the
  PDF versions of all vignettes.
  
  \code{pkgVignettes} returns an object of class \code{"pkgVignettes"}.
}
\keyword{utilities}
\keyword{documentation}
