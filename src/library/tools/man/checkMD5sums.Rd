% File src/library/tools/man/checkMD5sums.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{checkMD5sums}
\alias{checkMD5sums}
\title{Check and Create \file{MD5} Checksum Files}
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
  the bundle at \url{https://cran.r-project.org/bin/windows/Rtools/}.)

  If \code{dir} is missing, an installed package of name \code{package}
  is searched for.

  The private function \code{tools:::.installMD5sums} is used to create
  \file{MD5} files.
}
\note{
  As from \R 4.6.0, the use of \file{MD5} files has been deprecated in
  favor of \file{SHA256} files checked by \code{\link{checkSHA256sums}},
  since MD5 hashes are susceptible to hash-collision and length-extension
  attacks and therefore are only suitable for integrity checks and not
  for verification of authenticity.
}
\value{
  \code{checkMD5sums} returns a logical, \code{NA} if there is no
  \file{MD5} file to be checked.
}
\seealso{\code{\link{md5sum}}, \code{\link{checkSHA256sums}}}

\keyword{utilities}
