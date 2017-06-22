% File src/library/base/man/La_library.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2013-2017 R Core Team
% Distributed under GPL 2 or later

\name{La_library}
\alias{La_library}
\title{
  LAPACK Library
}
\description{
  Report the name of the shared object file with \code{LAPACK} implementation
  in use.
}
\usage{
La_library()
}
\value{
  A character vector of length one (\code{""} when the name is not known).
  The value can be used as an indication of which \code{LAPACK}
  implementation is in use.  Typically, the \R version of \code{LAPACK} will
  appear as \code{libRlapack.so} (\code{libRlapack.dylib}), depending on how
  R was built.  Note that \code{libRlapack.so} (\code{libRlapack.dylib}) may
  also be shown for an external \code{LAPACK} implementation that had been
  copied, hard-linked or renamed by the system administrator.  Otherwise,
  the shared object file will be given and its path/name may indicate
  the vendor/version.  The detection does not work on Windows.
}
\seealso{
  \code{\link{extSoftVersion}} for versions of other third-party software
  including \code{BLAS}.

  \code{\link{La_version}} for the version of LAPACK in use.
}
\examples{
La_library()
}
\keyword{utilities}
