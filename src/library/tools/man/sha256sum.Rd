% File src/library/tools/man/sha256sum.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{sha256sum}
\alias{sha256sum}
\title{Compute \abbr{SHA}-256 Checksums}
\description{
  Compute the 32-byte \abbr{SHA}-256 hashes of one or more files, or a raw vector of bytes.
}
\usage{
sha256sum(files, bytes)
}
\arguments{
  \item{files}{character. The paths of file(s) whose contents are to be hashed.}
  \item{bytes}{\code{\link{raw}}. Bytes to be hashed.
    NB: \code{bytes} and \code{files} are mutually exclusive.}
}
\details{
  A \abbr{SHA}-256 \sQuote{hash} or \sQuote{checksum} or \sQuote{message digest} is
  a 256-bit summary of the file contents represented by 64 hexadecimal
  digits.

  On Windows all files are read in binary mode (as the \code{sha256sum}
  utilities there do): on other OSes the files are read in the default
  mode (almost always text mode where there is more than one).
}
\value{
  If used with \code{files},
  a character vector of the same length as \code{files}, with names
  equal to \code{files} (possibly expanded).  The elements will be
  \code{NA} for non-existent or unreadable files, otherwise a
  64-character string of hexadecimal digits.
  
  For \code{bytes} the result is a single 64-character string.
}
\source{
  The underlying C code was written by \I{Ulrich Drepper}, extracted from
  the public domain version \file{SHA-crypt.txt} version 0.6 (2016-8-31).
}
\seealso{
  \code{\link{md5sum}}
}
\examples{
as.vector(sha256sum(dir(R.home(), pattern = "^COPY", full.names = TRUE)))
sha256sum(bytes=raw())
sha256sum(bytes=charToRaw("abc"))
}
\keyword{utilities}

