% File src/library/base/man/extSoftVersion.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2014 R Core Team
% Distributed under GPL 2 or later

\name{extSoftVersion}
\alias{extSoftVersion}
\title{
  Report Versions of Third-Party Software
}
\description{
  Report versions of (external) third-party software used.
}
\usage{
extSoftVersion()
}
\details{
  The reports the versions of third-party software libraries in use.
  These are often external but might have been compiled into \R which it
  was installed.

  With dynamic linking, these are the versions of the libraries linked
  to in this session.
}
\value{
  A named character vector, currently with components
  \item{zlib}{The version of \code{zlib} in use.}
  \item{bzlib}{The version of \code{bzlib} (from \command{bzip2}) in use.}
  \item{xz}{The version of \code{liblzma} (from \command{xz}) in use.}
  \item{PCRE}{The version of \code{PCRE} in use.}
  \item{ICU}{The version of \code{ICU} in use (if any).}
  \item{TRE}{The version of \code{libtre} in use.}

  All are character strings.  Note that the values for \code{bzlib} and
  \code{pcre} normally contain a date as well as the version number, and
  that for \code{tre} includes several items separated by spaces, the
  version number being the second.
}
\seealso{
  \code{\link{La_version}} for the version of LAPACK in use.

  \code{\link{grSoftVersion}} for third-party graphics software.
}
\examples{
extSoftVersion()
## the PCRE version
sub(" .*", "", extSoftVersion()["PCRE"])
}
