% File src/library/tools/man/checkMD5sums.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{checkMD5sums}
\alias{checkMD5sums}
\title{Check and Create MD5 Checksum Files}
\description{
  \code{checkMD5sums} checks the files against a file \file{MD5}.
}
\usage{
checkMD5sums(package, dir)
}
\arguments{
  \item{package}{the name of an installed package}
  \item{dir}{the path to the top-level directory of an installed
    package.}
}
\details{
  The file \file{MD5} which is created is in a format which can be
  checked by \command{md5sum -c MD5} if a suitable command-line version
  of \command{md5sum} is available.  (For Windows, one is supplied in
  the bundle at \url{http://CRAN.R-project.org/bin/windows/Rtools}.)

  If \code{dir} is missing, an installed package of name \code{package}
  is searched for.

  The private function \code{tools:::.installMD5sums} is used to create
  \code{MD5} files in the Windows build.
}
\value{
  \code{checkMD5sums} returns a logical, \code{NA} if there is no
  \file{MD5} file to be checked.
}
\seealso{\code{\link{md5sum}}}

\keyword{utilities}
