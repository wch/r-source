\name{vignette}
\alias{vignette}
\title{View or List Vignettes}
\description{
  View a specified vignette, or list the available ones.
}
\usage{
vignette(topic, package = .packages(), lib.loc = NULL)
}
\arguments{
  \item{topic}{a character string giving the (base) name of the vignette
    to view.}
  \item{package}{a character vector giving the packages to look into for
    vignettes.  By default, all packages in the search path are used.}
  \item{lib.loc}{a character vector of directory names of \R libraries,
    or \code{NULL}.  The default value of \code{NULL} corresponds to all
    libraries currently known.  If the default is used, the loaded
    packages are searched before the libraries.}
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
  
  
