\name{installed.packages}
\alias{installed.packages}
\title{Find Installed Packages}
\description{
  Find details of all packages installed in the specified libraries.
}
\usage{
installed.packages(lib.loc = NULL, priority = NULL)
}
\arguments{
  \item{lib.loc}{
    character vector describing the location of \R library trees to
    search through.
  }
  \item{priority}{
    character vector or \code{NULL} (default).  If non-null, used to
    select packages; \code{"high"} is equivalent to
    \code{c("base","recommended")}.  To select all packages without an
    assigned priority use \code{priority = "NA"}.
  }
}
\details{
  \code{installed.packages} scans the \file{DESCRIPTION} files of each
  package found along \code{lib.loc} and returns a matrix of package
  names, library paths and version numbers.
}
\value{
  A matrix with one row per package, row names the package names and
  column names \code{"Package"}, \code{"LibPath"}, \code{"Version"},
  \code{"Priority"}, \code{"Bundle"}, \code{"Depends"} and \code{"Suggests"}.
}
\seealso{
  \code{\link{update.packages}}
}
\examples{
str(ip <- installed.packages(priority = "high"))
ip[, c(1,3:5)]
}
\keyword{utilities}
