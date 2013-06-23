\name{getVignetteInfo}
\alias{getVignetteInfo}
\title{
Get information on installed vignettes.
}
\description{
This function gets information on installed vignettes.
}
\usage{
getVignetteInfo(package = NULL, lib.loc = NULL, all = TRUE)
}
\arguments{
  \item{package}{
Which package to look in, or \code{NULL} for all packages.
}
  \item{lib.loc}{
Which library to look in.
}
  \item{all}{
Whether to search all installed packages, or just attached packages.
}
}
\value{
A matrix with columns
\item{Package}{the name of the package}
\item{Dir}{the directory where the package is installed}
\item{File}{the base filename of the source of the vignette}
\item{Title}{the title of the vignette}
\item{R}{the tangled R source from the vignette}
\item{PDF}{the PDF or HTML file for display}
}
\note{
The last column of the result is named \code{PDF} for historical reasons, 
but it may contain a filename of a PDF or HTML document.
}
\seealso{
\code{\link{pkgVignettes}} is a similar function that can work on
an uninstalled package.
}
\examples{
\donttest{
getVignetteInfo("grid")
}
}
\keyword{ utilities }
\keyword{ documentation }
