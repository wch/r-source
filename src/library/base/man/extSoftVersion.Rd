% File src/library/base/man/extSoftVersion.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2014-2019 R Core Team
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
  These are often external but might have been compiled into \R when it
  was installed.

  With dynamic linking, these are the versions of the libraries linked
  to in this session: with static linking, of those compiled in.
}
\value{
  A named character vector, currently with components
  \item{zlib}{The version of \code{zlib} in use.}
  \item{bzlib}{The version of \code{bzlib} (from \command{bzip2}) in use.}
  \item{xz}{The version of \code{liblzma} (from \command{xz}) in use.}
  \item{PCRE}{The version of \code{PCRE} in use. PCRE1 has versions < 10.00,
  PCRE2 has versions >= 10.00.}
  \item{ICU}{The version of \code{ICU} in use (if any, otherwise \code{""}).}
  \item{TRE}{The version of \code{libtre} in use.}
  \item{iconv}{The implementation and version of the \code{iconv}
  library in use (if known).}
  \item{readline}{The version of \code{readline} in use (if any,
  otherwise \code{""}).}
  \item{BLAS}{Name of the binary/executable file with the implementation of
  \code{BLAS} in use (if known, otherwise \code{""}).}

  Note that the values for \code{bzlib} and \code{pcre} normally contain
  a date as well as the version number, and that for \code{tre} includes
  several items separated by spaces, the version number being the
  second.

  For \code{iconv} this will give the implementation as well as the
  version, for example \code{"GNU libiconv 1.14"}, \code{"glibc
  2.18"} or \code{"win_iconv"} (which has no version number).

  The name of the binary/executable file for \code{BLAS} can be used as an
  indication of which implementation is in use.  Typically, the R version of
  BLAS will appear as \code{libR.so} (\code{libR.dylib}), \code{R} or
  \code{libRblas.so} (\code{libRblas.dylib}), depending on how R was built.
  Note that \code{libRblas.so} (\code{libRblas.dylib}) may also be shown for
  an external BLAS implementation that had been copied, hard-linked or
  renamed by the system administrator.  For an external BLAS, a shared
  object file will be given and its path/name may indicate the
  vendor/version. The detection does not work on Windows.
}
\seealso{
  \code{\link{libcurlVersion}} for the version of \code{libCurl}.
  
  \code{\link{La_version}} for the version of LAPACK in use.

  \code{\link{La_library}} for binary/executable file with LAPACK in use.

  \code{\link{grSoftVersion}} for third-party graphics software.

  \code{\link{tclVersion}} for the version of Tcl/Tk.
  
  \code{\link{pcre_config}} for PCRE configuration options.
}
\examples{
extSoftVersion()
## the PCRE version
sub(" .*", "", extSoftVersion()["PCRE"])
}
