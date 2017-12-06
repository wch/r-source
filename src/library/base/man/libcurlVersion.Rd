% File src/library/base/man/curlVersion.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2015-2017 R Core Team
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

  \item{ssl_version}{A character string naming the SSL/TLS implementation
    and version, possibly \code{"none"}.  It is intended for the version
    of OpenSSL used, but not all implementations of \code{libcurl} use
    OpenSSL --- for example macOS reports \code{"SecureTranspart"}, its
    wrapper for SSL/TLS.}

  \item{libssh_version}{A character string naming the \code{libssh} version,
    which may or may not be available (it is used for
    e.g.\sspace{}\code{scp} and \code{sftp} protocols). Where present,
    something like \code{"libssh2/1.5.0"}.}

  \item{protocols}{A character vector of the names of supported
    protocols, also known as \sQuote{schemes} when part of a URL.}
}
\section{Warning}{
  In late 2017 a \code{libcurl} installation was seen divided into two
  libraries, \code{libcurl} and \code{libcurl-feature}, and the first
  had been updated but not the second.  As the compiled function
  recording the version was in the latter, the version reported by
  \code{libcurlVersion} was misleading.
}
\seealso{
  \code{\link{extSoftVersion}} for versions of other third-party
  software.

  \code{\link{curlGetHeaders}}, \code{\link{download.file}} and
  \code{\link{url}} for functions which (optionally) use \code{libcurl}.

  \url{http://curl.haxx.se/docs/sslcerts.html} and
  \url{http://curl.haxx.se/docs/ssl-compared.html} for more details on
  SSL versions (the current standard being known as TLS).  Normally
  \code{libcurl} used with \R uses SecureTransport on macOS, OpenSSL on
  Windows and GnuTLS, NSS or OpenSSL on Unix-alikes.  (At the time of
  writing Debian-based Linuxen use GnuTLS and RedHat-based ones use NSS,
  but it has been announced that Fedora 27 will switch to OpenSSL.)
}
\examples{
libcurlVersion()
}
