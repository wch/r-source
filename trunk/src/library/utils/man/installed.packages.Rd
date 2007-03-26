\name{installed.packages}
\alias{installed.packages}
\title{Find Installed Packages}
\description{
  Find (or retrieve) details of all packages installed in the specified
  libraries.
}
\usage{
installed.packages(lib.loc = NULL, priority = NULL,
                   noCache = FALSE, fields = NULL)
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

  \item{fields}{a character vector giving the fields to extract from
    each package's \code{DESCRIPTION} file in addition to the default
    ones, or \code{NULL} (default).  Unavailable fields result in
    \code{NA} values.}
}
\details{
  \code{installed.packages} scans the \file{DESCRIPTION} files of each
  package found along \code{lib.loc} and returns a matrix of package
  names, library paths and version numbers.

  \strong{Note:} this works with package names, not bundle names, and
  for versioned installs with the name under which the package is
  installed, in the style \code{mypkg_1.3-7}.

  The information found is cached (by library) for the \R session and
  specified \code{fields} argument, and updated only if the top-level
  library directory has been altered, for example by installing or
  removing a package.  If the cached information becomes confused, it
  can be refreshed by running \code{installed.packages(noCache =
  TRUE)}.
}
\value{
  A matrix with one row per package, row names the package names and
  column names \code{"Package"}, \code{"LibPath"}, \code{"Version"},
  \code{"Priority"}, \code{"Bundle"}, \code{"Contains"},
  \code{"Depends"}, \code{"Suggests"}, \code{"Imports"} and
  \code{"Built"} (the \R version the package was built under).
  Additional columns can be specified using the \code{fields}
  argument.
}
\seealso{
  \code{\link{update.packages}}, \code{\link{INSTALL}}, \code{\link{REMOVE}}.
}
\examples{
str(ip <- installed.packages(priority = "high"))
ip[, c(1,3:5)]
plic <- installed.packages(priority = "high", fields="License")
## what licenses are there:
table( plic[,"License"] )
}
\keyword{utilities}
