\name{urltools}
\alias{check_package_urls}
\title{Check Package URLs}
\description{
  Check URLs in package sources.
}
\usage{
check_package_urls(dir, verbose = FALSE)
}
\arguments{
  \item{dir}{a character string specifying the path to a package's root 
    source directory.}
  \item{verbose}{a logical indicating if output should monitor check
    progress.}
}
\details{
  Checking URLs is performed in parallel using \CRANpkg{curl} (so this
  must be installed for checking).

  The URLs checked are extracted from the package \file{DESCRIPTION}
  (\samp{Description}, \samp{URL} and \samp{BugReports} fields), Rd,
  \file{CITATION} and \file{NEWS.md} files, if package \CRANpkg{xml2} is
  installed also from the \file{.html} files in \file{inst/doc}, and if
  additionally \command{pandoc} is available on the system the
  \file{README.md} and \file{NEWS.md} files.

  For efficiency reasons, URLs are first checked using \samp{HEAD}
  requests, and the ones with non-OK (200) status codes re-checked via
  \samp{GET} requests (unfortunately, \samp{HEAD} requests are not
  always honored appropriately).
}
\value{
  A data frame inheriting from class \code{check_url_db} with
  information on the URL check problems found.
}
\note{
  This functionality is still experimental: interfaces may change in
  future versions. 
}
\keyword{utilities}
