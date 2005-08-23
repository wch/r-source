\name{installed.packages}
\alias{installed.packages}
\title{Find Installed Packages}
\description{
  Find (or retrieve) details of all packages installed in the specified
  libraries.
}
\usage{
installed.packages(lib.loc = NULL, priority = NULL, noCache = FALSE)
}
\arguments{
  \item{lib.loc}{
    character vector describing the location of \R library trees to
    search through.
  }
  \item{priority}{
    character vector or \code{NULL} (default).  If non-null, used to
    select packages; \code{"high"} is equivalent to
    \code{c("base", "recommended")}.  To select all packages without an
    assigned priority use \code{priority = "NA"}.
  }
  \item{noCache}{Do not use cached information.}
}
\details{
  \code{installed.packages} scans the \file{DESCRIPTION} files of each
  package found along \code{lib.loc} and returns a matrix of package
  names, library paths and version numbers.

  \strong{Note:} this works with package names, not bundle names.

  The information found is cached (by library) for the \R session,
  and updated only if the top-level library directory has been altered,
  for example by installing or removing a package.  If the cached
  information becomes confused, it can be refreshed by running
  \code{installed.packages(noCache = TRUE)}.
}
\value{
  A matrix with one row per package, row names the package names and
  column names \code{"Package"}, \code{"LibPath"}, \code{"Version"},
  \code{"Priority"}, \code{"Bundle"}, \code{"Contains"}, \code{"Depends"},
  \code{"Suggests"}, \code{"Imports"} and \code{"Built"}
  (the \R version the package was built under).
}
\seealso{
  \code{\link{update.packages}}, \code{\link{INSTALL}}, \code{\link{REMOVE}}.
}
\examples{
str(ip <- installed.packages(priority = "high"))
ip[, c(1,3:5)]
}
\keyword{utilities}
