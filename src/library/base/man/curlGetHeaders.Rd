% File src/library/base/man/curlGetHeaders.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2015-2023 R Core Team
% Distributed under GPL 2 or later

\name{curlGetHeaders}
\alias{curlGetHeaders}
\title{
  Retrieve Headers from URLs
}
\description{
  Retrieve the headers for a URL for a supported protocol such as
  \samp{http://}, \samp{ftp://}, \samp{https://} and \samp{ftps://}.
}
\usage{
curlGetHeaders(url, redirect = TRUE, verify = TRUE,
               timeout = 0L, TLS = "")
}

\arguments{
  \item{url}{character string specifying the URL.}
  \item{redirect}{logical: should redirections be followed?}
  \item{verify}{logical: should certificates be verified as valid
    and applying to that host?}
  \item{timeout}{integer: the maximum time in seconds the request is
    allowed to take.  Non-positive and invalid values are ignored
    (including the default).  (Added in \R 4.1.0.)}
  \item{TLS}{character: the minimum version of the \abbr{TLS} protocol to be used
    for \samp{https://} URLs: the default (\code{""}) is no restriction
    beyond that of the underlying \code{libcurl} (usually 1.0).  Other
    valid values are \code{"1.1"}, \code{"1.2"} (both for \code{libcurl}
    7.34.0 and later)  and \code{"1.3"} (7.52.0 and later), if supported
    by the underlying version of \code{libcurl} and the \abbr{SSL} library it uses.}
  %% 1.0 and 1.1 were deprecated in 2020.
  %% e.g. libcurl in macOS 10.15 supported 1.3 but its LibreSSL does not.
}

\details{
  This reports what \command{curl -I -L} or \command{curl -I} would
  report.  For a \samp{ftp://} URL the \sQuote{headers} are a record of
  the conversation between client and server before data transfer.

  Only 500 header lines will be reported: there is a limit of 20
  redirections so this should suffice (and even 20 would indicate
  problems).

  If argument \code{timeout} is not set to a positive integer this uses
  \code{\link{getOption}("timeout")} which defaults to 60 seconds.  As
  the request cannot be interrupted you may want to consider a shorter
  value.

  To see all the details of the interaction with the server(s) set
  \code{\link{options}(internet.info = 1)}.
  
  HTTP[S] servers are allowed to refuse requests to read the headers and
  some do: this will result in a \code{status} of \code{405}.
  
  For possible issues with secure URLs (especially on Windows) see
  \code{\link{download.file}}.

  There is a security risk in not verifying certificates, but as only
  the headers are captured it is slight.  Usually looking at the URL in
  a browser will reveal what the problem is (and it may well be
  machine-specific).
}
\value{
  A character vector with integer attribute \code{"status"} (the
  last-received \sQuote{status} code).  If redirection occurs this will include
  the headers for all the URLs visited.

  For the interpretation of \sQuote{status} codes see
  \url{https://en.wikipedia.org/wiki/List_of_HTTP_status_codes} and
  \url{https://en.wikipedia.org/wiki/List_of_FTP_server_return_codes}.
  A successful FTP connection will usually have status 250, 257 or 350.  
}

\seealso{
  \code{\link{capabilities}("libcurl")} to see if this is supported.
  \code{\link{libcurlVersion}} for the version of \code{libcurl} in use.

  \code{\link{options}} \code{HTTPUserAgent} and \code{timeout} are used.
}

\examples{\donttest{## needs Internet access, results vary
curlGetHeaders("http://bugs.r-project.org")   ## this redirects to https://
## 2023-04: replaces slow and unreliable https://httpbin.org/status/404
curlGetHeaders("https://developer.R-project.org/inet-tests/not-found")
## returns status
}}
