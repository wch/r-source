\name{packageStatus}
\title{Package Management Tools}
\alias{compareVersion}
\alias{newestVersion}
\alias{packageStatus}
\alias{print.packageStatus}
\alias{summary.packageStatus}
\alias{update.packageStatus}
\alias{upgrade.packageStatus}
\alias{upgrade}
\description{Summarize information about installed packages and packages
  available at various repositories, and automatically upgrade outdated
  packages. These tools will replace \code{\link{update.packages}} and friends
  in the future and are currently work in progress.}
\usage{
packageStatus(lib.loc = NULL, repositories = getOption("repositories"))
\method{summary}{packageStatus}(object, \dots)
\method{update}{packageStatus}(object, lib.loc = levels(object$inst$LibPath),
       repositories = levels(object$avail$Repository), \dots)
\method{upgrade}{packageStatus}(object, ask = TRUE, \dots)
}
\arguments{
  \item{lib.loc}{a character vector describing the location of \R
    library trees to search through, or \code{NULL}.  The default value
    of \code{NULL} corresponds to all libraries currently known.}
  \item{repositories}{a character vector of URLs describing the location of \R
    package repositories on the Internet or on the local machine.}
  \item{object}{return value of \code{packageStatus}.}
  \item{ask}{if \code{TRUE}, the user is prompted which packages should
    be upgraded and which not.}
  \item{\dots}{currently not used.}
}
\examples{
\dontrun{
x <- packageStatus()
print(x)
summary(x)
upgrade(x)
x <- update(x)
print(x)
}}
\keyword{utilities}
