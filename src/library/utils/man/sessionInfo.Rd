% File src/library/utils/man/sessionInfo.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{sessionInfo}
\title{Collect Information About the Current R Session}
\alias{sessionInfo}
\alias{toLatex.sessionInfo}
\alias{print.sessionInfo}
\description{
  Print version information about \R and attached or loaded packages.
}
\usage{
sessionInfo(package = NULL)
\S3method{print}{sessionInfo}(x, locale = TRUE, ...)
\S3method{toLatex}{sessionInfo}(object, locale = TRUE, ...)
}
\arguments{
 \item{package}{a character vector naming installed packages, or \code{NULL}
   (the default) meaning all attached packages.}
  \item{x}{an object of class \code{"sessionInfo"}.}
  \item{object}{an object of class \code{"sessionInfo"}.}
  \item{locale}{show locale information?}
  \item{\dots}{currently not used.}
}
\value{
  An object of class \code{"sessionInfo"}, which has a \code{print}
  method. This is a list with components
  \item{R.version}{a list, the result of calling \code{\link{R.Version}()}.}
  \item{platform}{a character string describing the platform.  For recent
    versions where sub-architectures are in use this is of the form
    \samp{platform/sub-arch (nn-bit)}.}
  \item{locale}{a character string, the result of calling
    \code{\link{Sys.getlocale}()}.}
  \item{basePkgs}{a character vector of base packages which are attached.}
  \item{otherPkgs}{(not always present): a character vector of other
    attached packages.}
  \item{loadedOnly}{(not always present): a named list of the results of
    calling \code{\link{packageDescription}} on packages whose
    namespaces are loaded but are not attached.}
}
\note{
  The information on \sQuote{loaded} packages and namespaces is the
  \emph{current} version installed at the location the package was
  loaded from: it can be wrong if another process has been changing
  packages during the session.
}
\seealso{\code{\link{R.version}}
}
\examples{\donttest{
sessionInfo()
toLatex(sessionInfo(), locale = FALSE)
}}
\keyword{misc}
