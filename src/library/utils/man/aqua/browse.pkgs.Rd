\name{browse.pkgs}
\alias{browse.pkgs}
\alias{browse.install.pkgs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Download R packages}
\description{
    Visual browsers for downloading R binary or source p ackages from a
  repository, usually invoked from the Packages menu. \code{browse.pkgs}
  shows all available packages, \code{browse.update.pkgs} shows only
  installed packages with different version numbers on the repository.
}
\usage{
browse.pkgs(where = c("CRAN", "BIOC"), type = c("binary", "source"), contriburl, global = FALSE)
browse.update.pkgs(where = c("CRAN", "BIOC"), type = c("binary", "source"), in.place = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{where}{Browse \code{"CRAN"} or \code{"BIOConductor"}}
  \item{type}{Type of package to download}
  \item{contriburl}{Different URL to browse. Overrides \code{where}}
  \item{global}{Not implemented}
  \item{in.place}{Put the new package in the same directory as the old version}
}
\details{
The \code{CRAN} and \code{BIOC} URLs are set in R with Aqua GUI
Preferences. Packages are install in \code{.libPaths()[1]}, which is
typically \code{~/Library/R/library}.
}
\value{
  Used for its side effects
}
\seealso{\code{install.binaries}, \code{install.packages}, \code{update. }
\examples{

}
\keyword{connection}% at least one, from doc/KEYWORDS
\keyword{utilities}% __ONLY ONE__ keyword per line
