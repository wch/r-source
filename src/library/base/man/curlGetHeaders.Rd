% File src/library/base/man/curlGetHeaders.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2015 R Core Team
% Distributed under GPL 2 or later

\name{curlGetHeaders}
\alias{curlGetHeaders}
\title{
  Retrieve Headers from URLs
}
\description{
  Retrieve the headers for a URL for a supported protocol such as
  \code{http://}, \code{ftp://} and \code{https://}.
}
\usage{
curlGetHeaders(url, redirect = TRUE)
}

\arguments{
  \item{url}{character string specifying the URL.}
  \item{redirect}{logical: should redirections be followed?}
}

\details{
  This  reports what \command{curl -I -L} or \command{curl -I} would
  report.

  Only 500 header lines will be reported: there is a limit of 20
  redirections so this should suffice (and even 20 would indicate
  problems).

  It uses \code{\link{getOption}("timeout")} for the connection
  timeout: that defaults to 60 seconds.  As this cannot be interrupted
  you may want to consider a shorter value.

  For possible issues with secure URLs (especially on Windows) see
  \code{\link{download.file}}.
}
\value{
  A character vector with integer attribute \code{"status"} (the
  last-received status code).  If redirection occurs this will include
  the headers for all the URLs visited.
}

\seealso{
  \code{\link{capabilities}("libcurl")} to see if this is supported.
}

\examples{\donttest{## needs Internet access, results vary
curlGetHeaders("http://bugs.r-project.org")   ## this redirects to https://
curlGetHeaders("https://httpbin.org/status/404")  ## returns status
curlGetHeaders("ftp://cran.r-project.org")
}}
