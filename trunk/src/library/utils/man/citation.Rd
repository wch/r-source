\name{citation}
\title{Citing R and R Packages in Publications}
\alias{citation}
\alias{toBibtex.citation}
\alias{toBibtex.citationList}
\description{
  How to cite R and R packages in publications.
}
\usage{
citation(package = "base", lib.loc = NULL)
\S3method{toBibtex}{citation}(object, \dots)
\S3method{toBibtex}{citationList}(object, \dots)
}
\arguments{
  \item{package}{a character string with the name of a single package.
    An error occurs if more than one package name is given.}
  \item{lib.loc}{a character vector with path names of \R libraries, or
    \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages are searched before the libraries.}
  \item{object}{return object of \code{citation}.}
  \item{\dots}{currently not used.}
}
\details{
  The R core development team and the very active community of package
  authors have invested a lot of time and effort in creating R as it is
  today. Please give credit where credit is due and cite R and R
  packages when you use them for data anlysis.
  
  Execute function \code{citation()} for information on how to cite the
  base R system in publications. If the name of a non-base package is
  given, the function
  either returns the information contained in the \code{\link{CITATION}}
  file of the package or auto-generates citation information. In the
  latter case the package \file{DESCRIPTION} file is parsed, the
  resulting citation object may be arbitrarily bad, but is quite useful
  (at least as a starting point) in most cases.

  If only one reference is given, the print method shows both a text
  version and a BibTeX entry for it, if a package has more than one
  reference then only the text versions are shown. The BibTeX versions
  can be obtained using function \code{toBibtex} (see the examples below).
}
\value{
  An object of class \code{"citationList"}.
}
\seealso{\code{\link{citEntry}}}
\keyword{misc}
\examples{
## the basic R reference
citation()

## references for a package -- might not have these installed
if(nchar(system.file(package="lattice"))) citation("lattice")
if(nchar(system.file(package="foreign"))) citation("foreign")

## extract the bibtex entry from the return value
x <- citation()
toBibtex(x)
}
