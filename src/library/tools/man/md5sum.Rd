% File src/library/tools/man/md5sum.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{md5sum}
\alias{md5sum}
\title{Compute MD5 Checksums}
\description{
  Compute the 32-byte MD5 checksums of one or more files.
}
\usage{
md5sum(files)
}
\arguments{
  \item{files}{character. The paths of file(s) to be check-summed.}
}
\value{
  A character vector of the same length as \code{files}, with names
  equal to \code{files}. The elements
  will be \code{NA} for non-existent or unreadable files, otherwise
  a 32-character string of hexadecimal digits.

  On Windows all files are read in binary mode (as the \code{md5sum}
  utilities there do): on other OSes the files are read in the default way.
}
\seealso{
  \code{\link{checkMD5sums}}
}
\examples{
as.vector(md5sum(dir(R.home(), pattern = "^COPY", full.names = TRUE)))
}
\keyword{utilities}

