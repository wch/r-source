% File src/library/tools/man/checkMD5sums.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{checkSHA256sums}
\alias{checkSHA256sums}
\title{Check and Create SHA256 Checksum Files}
\description{
  \code{checkSHA256sums} checks the files against a file \file{SHA256}.
}
\usage{
checkSHA256sums(package, dir)
}
\arguments{
  \item{package}{the name of an installed package}
  \item{dir}{the path to the top-level directory of an installed
    package.}
}
\details{
  The file \file{SHA256} which is a text file with the following
  entries separated by a single space each: hash (64-digit hexadecimal
  representation of the SHA256 hash), size of the file in bytes and
  the file path.

  If \code{dir} is missing, an installed package of name \code{package}
  is searched for.

  The private function \code{tools:::.installSHA256sums} is used to create
  \code{SHA256} files.
}
\value{
  \code{checkSHA256sums} returns a logical, \code{NA} if there is no
  \file{SHA256} file to be checked.
}
\seealso{\code{\link{sha256sum}}, \code{\link{checkMD5sums}}}

\keyword{utilities}
