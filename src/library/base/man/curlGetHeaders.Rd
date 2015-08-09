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
  \code{http://}, \code{ftp://}, \code{https://} and \code{ftps://}.
  An optional function not supported on all platforms.
}
\usage{
curlGetHeaders(url, redirect = TRUE, verify = TRUE)
}

\arguments{
  \item{url}{character string specifying the URL.}
  \item{redirect}{logical: should redirections be followed?}
  \item{verify}{logical: should certificates be verified as valid
    and applying to that host?}
}

\details{
  This reports what \command{curl -I -L} or \command{curl -I} would
  report.  For a \code{ftp://} URL the \sQuote{headers} are a record of
  the conversation between client and server before data transfer.

  Only 500 header lines will be reported: there is a limit of 20
  redirections so this should suffice (and even 20 would indicate
  problems).

  It uses \code{\link{getOption}("timeout")} for the connection
  timeout: that defaults to 60 seconds.  As this cannot be interrupted
  you may want to consider a shorter value.

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
  \url{http://en.wikipedia.org/wiki/List_of_HTTP_status_codes} and
  \url{http://en.wikipedia.org/wiki/List_of_FTP_server_return_codes}.
  A successful FTP connection will usually have status 250 or 350.  
}

\seealso{
  \code{\link{capabilities}("libcurl")} to see if this is supported.

  \code{\link{options}} \code{HTTPUserAgent} and \code{timeout} are used.
}

% http://forum.rebex.net/questions/1343/open-ftps-and-sftp-servers-for-testing-our-code-and-connectivity
\examples{\donttest{## needs Internet access, results vary
curlGetHeaders("http://bugs.r-project.org")   ## this redirects to https://
curlGetHeaders("https://httpbin.org/status/404")  ## returns status
curlGetHeaders("ftp://cran.r-project.org")
}
\dontrun{ ## a not-always-available site:
curlGetHeaders("ftps://test.rebex.net/readme.txt")
}}
