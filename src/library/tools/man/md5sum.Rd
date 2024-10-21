% File src/library/tools/man/md5sum.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{md5sum}
\alias{md5sum}
\title{Compute MD5 Checksums}
\description{
  Compute the 32-byte MD5 hashes of one or more files, or a raw vector of bytes.
}
\usage{
md5sum(files, bytes)
}
\arguments{
  \item{files}{character. The paths of file(s) whose contents are to be hashed.}
  \item{bytes}{raw. Bytes to be hashed.
    NB: \code{bytes} and \code{files} are mutually exclusive.}
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
  correctly and not subsequently accidentally modified.
}
\value{
  If used with \code{files},
  a character vector of the same length as \code{files}, with names
  equal to \code{files} (possibly expanded).  The elements will be
  \code{NA} for non-existent or unreadable files, otherwise a
  32-character string of hexadecimal digits.
  
  For \code{bytes} the result is a single 32-character string.
}
\source{
  The underlying C code was written by \I{Ulrich Drepper} and extracted from
  a 2001 release of \code{glibc}.
}
\seealso{
  \code{\link{checkMD5sums}}, \code{\link{sha256sum}}
}
\examples{
as.vector(md5sum(dir(R.home(), pattern = "^COPY", full.names = TRUE)))
md5sum(bytes=raw())
md5sum(bytes=charToRaw("abc"))
}
\keyword{utilities}

