\name{urltools}
\alias{check_package_urls}
\alias{parse_URI_reference}
\title{Check Package URLs}
\description{
  Check URLs in package sources.
}
\usage{
check_package_urls(dir, verbose = FALSE)
parse_URI_reference(x)
}
\arguments{
  \item{dir}{a character string specifying the path to a package's root 
    source directory.}
  \item{verbose}{a logical indicating if output should monitor check
    progress.}
  \item{x}{a character vector of \abbr{URI}s.}
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

  \code{parse_URI_reference()} is a helper which splits URI references
  into its scheme, authority, path, query and fragment parts.
  See RFC 3986 (\url{https://www.rfc-editor.org/rfc/rfc3986}) for more
  information.
}
\value{
  For \code{check_url_db()}, a data frame inheriting from class
  \code{check_url_db} with information on the URL check problems found.

  For \code{parse_URI_reference()}, a data frame with character
  variables \code{scheme}, \code{authority}, \code{path}, \code{query}
  and \code{fragment}. 
}
\note{
  This functionality is still experimental: interfaces may change in
  future versions. 
}
\examples{
## Examples from RFC 3986.
parse_URI_reference(c("foo://example.com:8042/over/there?name=ferret#nose",
                      "urn:example:animal:ferret:nose",
                      "mailto:John.Doe@example.com",
                      "tel:+1-816-555-1212"))
}
\keyword{utilities}
