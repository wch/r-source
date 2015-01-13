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
  A character string, with value the \code{libcurl} version in use, or
  \code{""} if none is.  If \code{libcurl} is available, has attributes

  \item{ssl_version}{A character string naming the SSL version, possibly
  \code{"none"}.}
  \item{libssh_version}{A character string naming the libssh version,
    which may or may not be available.}
  \item{protocols}{A character vector of the names of supported protocols.}
}
\seealso{
  \code{\link{extSoftVersion}} for versions of other third-party software.
}
\examples{
libcurlVersion()
}
