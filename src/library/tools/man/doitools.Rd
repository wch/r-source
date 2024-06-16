\name{doitools}
\alias{check_package_dois}
\title{Check Package \abbr{DOI}s}
\description{
  Check \abbr{DOI}s in package sources.
}
\usage{
check_package_dois(dir, verbose = FALSE)
}
\arguments{
  \item{dir}{a character string specifying the path to a package's root 
    source directory.}
  \item{verbose}{a logical indicating if output should monitor check
    progress.}
}
\details{
  Checking \abbr{DOI}s is performed in parallel using \CRANpkg{curl} (so this
  must be installed for checking).

  The \abbr{DOI}s checked are extracted from the package \file{DESCRIPTION}
  (\samp{<doi:...>} \abbr{URI}s in the \samp{Description} field), Rd and
  \file{CITATION} files.

  The \abbr{DOI}s are checked via \samp{HEAD} requests to the DOI system Proxy
  Server REST API.
}
\value{
  A data frame inheriting from class \code{check_doi_db} with
  information on the DOI check problems found.
}
\note{
  This functionality is still experimental: interfaces may change in
  future versions. 
}
\keyword{utilities}
