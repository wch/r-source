% File src/library/base/man/La_version.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2013-2022 R Core Team
% Distributed under GPL 2 or later

\name{La_version}
\alias{La_version}
\title{
  LAPACK Version
}
\description{
  Report the version of LAPACK in use.
}
\usage{
La_version()
}
\value{
  A character vector of length one.

  Note that this is the version as reported by the library at runtime.
  It may differ from the reference (\sQuote{\I{netlib}}) implementation, for
  example by having some optimized or patched routines.  For the version
  included with \R, the older (not Fortran 90) versions of
  \preformatted{
    DLARTG DLASSQ ZLARTG ZLASSQ
  }
  are used.
}
\seealso{
  \code{\link{extSoftVersion}} for versions of other third-party software.

  \code{\link{La_library}} for binary/executable file with LAPACK in use.
}
\examples{
La_version()
}
\keyword{utilities}
