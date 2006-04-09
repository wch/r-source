\name{checkMD5sums}
\alias{checkMD5sums}
\title{Check and Create MD5 Checksum Files}
\description{
  \code{checkMD5sums} checks the files against a file \code{MD5}.
}
\usage{
checkMD5sums(pkg, dir)
}
\arguments{
  \item{pkg}{the name of an installed package}
  \item{dir}{the path to the top-level directory of an installed
    package.}
}
\details{
  The file \file{MD5} which is created is in a format which can be
  checked by \code{md5sum -c MD5} if a suitable command-line version of
  \code{md5sum} is available.  (One is supplied in the bundle at
  \url{http://www.murdoch-sutherland.com/Rtools/tools.zip}.)

  If \code{dir} is missing, an installed package of name \code{pkg} is
  searched for.

  The private function \code{tools:::.installMD5sums} is used to create
  \code{MD5} files in the Windows build.
}
\value{
  \code{checkMD5sums} returns a logical, \code{NA} if there is no
  \code{MD5} file to be checked.
}
\seealso{\code{\link{md5sum}}}

\keyword{utilities}
