\name{vignette}
\alias{vignette}
\title{View or List Vignettes}
\description{
  View a specified vignette, or list the available ones.
}
\usage{
vignette(topic, package = NULL, lib.loc = NULL)
}
\arguments{
  \item{topic}{a character string giving the (base) name of the vignette
    to view.}
  \item{package}{a character vector with the names of packages to
    search through, or \code{NULL} in which case \emph{all} available
    packages in the library trees specified by \code{lib.loc} are
    searched.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.}
}
\details{
  Currently, only PDF versions of vignettes can be viewed.
#ifdef unix
  The program specified by the \code{pdfviewer} option is used for this.
#endif
  If several vignettes have PDF versions with base name identical to
  \code{topic}, the first one found is used for viewing.

  If no topics are given, the available vignettes are listed.  The
  corresponding information is returned in an object of class
  \code{"packageIQR"}.  The structure of this class is experimental.
}
\examples{
## List vignettes in all attached packages
vignette()
## List vignettes in all available packages
vignette(package = .packages(all.available = TRUE))
}
\keyword{documentation}
  
  
