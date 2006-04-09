\name{install.binaries}
\alias{install.binaries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Download and install binary packages }
\description{
Installs precompiled binary packages from a repository using the format
of CRAN or Bioconductor.  
}
\usage{
install.binaries(pkgs, lib, CRAN = getOption("CRAN"), contriburl = contrib.url(CRAN, type = "mac.binary"), method, available = NULL, destdir = NULL, installWithVers = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pkgs}{names of packages to install}
  \item{lib}{where to install them }
  \item{CRAN}{URL for root of repository (may be \code{file://} URL)}
  \item{contriburl}{URL of repository package directory }
  \item{method}{see \code{\link{download.file}}}
  \item{available}{list of packages available in the repository (optional,to save
  bandwidth)}
  \item{destdir}{Directory for downloaded files }
  \item{installWithVers}{Install with versioning information}
}
\value{
 The function is called for its side-effect of installing packages.
}

\seealso{\code{\link{install.packages}}, \code{\link{download.file}}}
\examples{

}
\keyword{connection}% at least one, from doc/KEYWORDS
\keyword{utilities}% __ONLY ONE__ keyword per line
