\name{checkMD5sums}
\alias{checkMD5sums}
\alias{.installMD5sums}
\title{Check and Create MD5 Checksum Files}
\description{
  \code{checkMD5sums} checks the files against a file \code{MD5};
  \code{.installMD5sums} creates such a file.
}
\usage{
checkMD5sums(pkg, dir)
.installMD5sums(pkgDir, outDir = pkgDir)
}
\arguments{
  \item{pkg}{the name of an installed package}
  \item{dir, pkgDir}{the path to the top-level directory of an installed
    package.}
  \item{outDir}{the directory within which to create the \code{MD5} file.}
}
\details{
  The file \file{MD5} which is created is in a format which can be
  checked by \code{md5sum -c MD5} if a suitable command-line version of
  \code{md5sum} is available.

  If \code{dir} is missing, an installed package of name \code{pkg} is
  searched for.
}
\value{
  \code{checkMD5sums} returns a logical, \code{NA} if there is no
  \code{MD5} file to be checked.
}
\seealso{\code{\link{md5sum}}}

\keyword{utilities}
