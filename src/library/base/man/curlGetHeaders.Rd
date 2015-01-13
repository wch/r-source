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

  It uses \code{\link{getOption}("timeout")} for the connection
  timeout: that defaults to 60 seconds.  As this cannot be interrupted
  you may want to consider a shorter value.
}
\value{
  A character vector with integer attribute \code{"status"} (the
  last-received status code).  If redirection occurs this will include
  the headers for all the URLs visited.
}

\seealso{
  \code{\link{capabilities}("libcurl")} to see if this is supported.
}

\examples{\donttest{## needs Internet access
curlGetHeaders("http://bugs.r-project.org")   ## this redirects
}}
