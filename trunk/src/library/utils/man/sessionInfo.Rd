\name{sessionInfo}
\title{Collect Information About the Current R Session}
\alias{sessionInfo}
\alias{toLatex.sessionInfo}
\alias{print.sessionInfo}
\description{
  Print version information about R and attached packages.
}
\usage{
sessionInfo(package=NULL)
\S3method{print}{sessionInfo}(x, ...)
\S3method{toLatex}{sessionInfo}(object, ...)
}
\arguments{
 \item{package}{a character vector naming installed packages.  By
   default all attached packages are used.}
  \item{x}{an object of class \code{"sessionInfo"}.}
  \item{object}{an object of class \code{"sessionInfo"}.}
  \item{\dots}{currently not used.}
}
\keyword{misc}
\examples{
sessionInfo()
toLatex(sessionInfo())
}
