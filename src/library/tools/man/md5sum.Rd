% File src/library/tools/man/md5sum.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{md5sum}
\alias{md5sum}
\title{Compute MD5 Checksums}
\description{
  Compute the 32-byte MD5 hashes of one or more files.
}
\usage{
md5sum(files)
}
\arguments{
  \item{files}{character. The paths of file(s) whose contents are to be hashed.}
}
\details{
  A MD5 \sQuote{hash} or \sQuote{checksum} or \sQuote{message digest} is
  a 128-bit summary of the file contents represented by 32 hexadecimal
  digits.  Files with different MD5 sums are different: only very
  exceptionally (and usually with the intent to deceive) are those with
  the same sums different.

  On Windows all files are read in binary mode (as the \code{md5sum}
  utilities there do): on other OSes the files are read in the default
  mode (almost always text mode where there is more than one).

  MD5 sums are used as a check that \R packages have been unpacked
  correctly and not subsequently modified.
}
\value{
  A character vector of the same length as \code{files}, with names
  equal to \code{files}.  The elements will be \code{NA} for non-existent
  or unreadable files, otherwise a 32-character string of hexadecimal
  digits.
}
\source{
  The underlying C code was written by Ulrich Drepper and extracted from
  a 2001 release of \code{glibc}.
}
\seealso{
  \code{\link{checkMD5sums}}
}
\examples{
as.vector(md5sum(dir(R.home(), pattern = "^COPY", full.names = TRUE)))
}
\keyword{utilities}

