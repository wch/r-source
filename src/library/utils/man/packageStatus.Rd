% File src/library/utils/man/packageStatus.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{packageStatus}
\title{Package Management Tools}
\alias{packageStatus}
\alias{print.packageStatus}
\alias{summary.packageStatus}
\alias{update.packageStatus}
\alias{upgrade.packageStatus}
\description{
  Summarize information about installed packages and packages
  available at various repositories, and automatically upgrade outdated
  packages.
}
\usage{
packageStatus(lib.loc = NULL, repositories = NULL, method,
              type = getOption("pkgType"), \dots)

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
    package repositories on the Internet or on the local machine.
    These should be full paths to the appropriate \file{contrib}
    sections of the repositories.
    The default (\code{NULL}) derives URLs from \link{option}
    \code{"repos"} and \code{type}.}
  \item{method}{download method, see \code{\link{download.file}}.}
  \item{type}{type of package distribution:
    see \code{\link{install.packages}}.}
  \item{object}{an object of class \code{"packageStatus"} as returned by
    \code{packageStatus}.}
  \item{ask}{if \code{TRUE}, the user is prompted which packages should
    be upgraded and which not.}
  \item{\dots}{for \code{packageStatus}: arguments to be passed to
    \code{\link{available.packages}} and \code{\link{installed.packages}}.\cr
    for the \code{upgrade} method, arguments to be passed to
    \code{\link{install.packages}}\cr
    for other methods: currently not used.}
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

  \item{inst}{a data frame with columns as the \emph{matrix} returned by
    \code{\link{installed.packages}} plus \code{"Status"}, a factor with
    levels \code{c("ok", "upgrade", "unavailable")}.  Only the newest 
    version of each package is reported, in the first repository in 
    which it appears.
  }

  \item{avail}{a data frame with columns as the \emph{matrix} returned by
    \code{\link{available.packages}} plus \code{"Status"}, a factor with
    levels \code{c("installed", "not installed")}.
  }

  For the \code{summary} method the result is also of class
  \code{"summary.packageStatus"} with additional components

  \item{Libs}{a list with one element for each library}
  \item{Repos}{a list with one element for each repository}

  with the elements being lists of character vectors of package name for
  each status.
}
\seealso{
  \code{\link{installed.packages}}, \code{\link{available.packages}}
}
\examples{
\donttest{
x <- packageStatus(repositories = contrib.url(findCRANmirror("web")))
print(x)
summary(x)
}
\dontrun{
upgrade(x)
x <- update(x)
print(x)
}}
\keyword{utilities}
