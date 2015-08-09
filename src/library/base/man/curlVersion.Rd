% File src/library/base/man/curlVersion.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2015 R Core Team
% Distributed under GPL 2 or later

\name{libcurlVersion}
\alias{libcurlVersion}
\title{
  Report Version of libcurl
}
\description{
  Report version of \code{libcurl} in use.
}
\usage{
libcurlVersion()
}
\value{
  A character string, with value the \code{libcurl} version in use or
  \code{""} if none is.  If \code{libcurl} is available, has attributes

  \item{ssl_version}{A character string naming the SSL version, possibly
  \code{"none"}.  It is intended for the version of OpenSSL used, but
  not all implementations of \code{libcurl} use OpenSSL --- for example
  OS X reports \code{"SecureTranspart"}, its wrapper for SSL/TLS.}
  \item{libssh_version}{A character string naming the \code{libssh} version,
    which may or may not be available (it is used for
    e.g.\sspace{}\code{scp} and \code{sftp} protocols). Where present,
    something like \code{"libssh2/1.5.0"}.}
  \item{protocols}{A character vector of the names of supported
    protocols, also known as \sQuote{schemes} when part of a URL.}
}
\seealso{
  \code{\link{extSoftVersion}} for versions of other third-party
  software.

  \code{\link{curlGetHeaders}}, \code{\link{download.file}} and
  \code{\link{url}} for functions which (optionally) use \code{libcurl}.
}
\examples{
libcurlVersion()
}
