\name{packageStatus}
\title{Package Management Tools}
\alias{packageStatus}
\alias{print.packageStatus}
\alias{summary.packageStatus}
\alias{update.packageStatus}
\alias{upgrade.packageStatus}
\alias{upgrade}
\description{
  Summarize information about installed packages and packages
  available at various repositories, and automatically upgrade outdated
  packages. These tools will replace \code{\link{update.packages}} and friends
  in the future and are currently work in progress.
}
\usage{
packageStatus(lib.loc = NULL,
              repositories = getOption("repositories")())

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
  \item{object}{an object of class \code{"packageStatus"} as returned by
    \code{packageStatus}.}
  \item{ask}{if \code{TRUE}, the user is prompted which packages should
    be upgraded and which not.}
  \item{\dots}{currently not used.}
}
\details{
  There are \code{print} and \code{summary} methods for the
  \code{"packageStatus"} objects: the \code{print} method gives a brief
  tabular summary and the \code{summary} method prints the results.
  
  The \code{update} method updates the \code{"packageStatus"} object.
  The \code{upgrade} method is similar to \code{\link{update.packages}}:
  it offers to install the current versions of those packages which are not
  currently up-to-date.
}
\value{
  An object of class \code{"packageStatus"}.  This is a list with two
  components

  \item{inst}{ a data frame with columns
    \code{"Package"}, \code{"Version"}, \code{"Priority"},
    \code{"Bundle"}, \code{"Depends"}, \code{"Built"}, \code{"Status"} and
    \code{"LibPath"}.
  }

  \item{avail}{a data frame  with columns
    \code{"Package"}, \code{"Version"}, \code{"Priority"},
    \code{"Bundle"}, \code{"Depends"}, \code{"Built"}, \code{"Status"} and
    \code{"Repository"} (the URL of the directory from which they can be
    fetched).
  }

  These have similar content to the \emph{matrices} returned by
  \code{\link{installed.packages}} and \code{\link{CRAN.packages}}.
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
