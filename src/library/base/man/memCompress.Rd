% File src/library/base/man/memCompress.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009-11 R Core Team
% Distributed under GPL 2 or later

\name{memCompress}
\alias{memCompress}
\alias{memDecompress}
\concept{gzip}
\concept{bzip2}
\concept{lzma}
\title{In-memory Compression and Decompression}
\description{
  In-memory compression or decompression for raw vectors.
}
\usage{
memCompress(from, type = c("gzip", "bzip2", "xz", "none"))

memDecompress(from,
              type = c("unknown", "gzip", "bzip2", "xz", "none"),
              asChar = FALSE)
}
\arguments{
  \item{from}{A raw vector.  For \code{memCompress} a character vector
    will be converted to a raw vector with character strings separated
    by \code{"\n"}.}
  \item{type}{character string, the type of compression.  May be
    abbreviated to a single letter, defaults to the first of the alternatives.}
  \item{asChar}{logical: should the result be converted to a character
    string?}
}

\details{
  \code{type = "none"} passes the input through unchanged, but may be
  useful if \code{type} is a variable.

  \code{type = "unknown"} attempts to detect the type of compression
  applied (if any): this will always succeed for \command{bzip2}
  compression, and will succeed for other forms if there is a suitable
  header.  It will auto-detect the \sQuote{magic} header
  (\code{"\x1f\x8b"}) added to files by the \command{gzip} program (and
  to files written by \code{\link{gzfile}}), but \code{memCompress} does
  not add such a header.

  \command{bzip2} compression always adds a header (\code{"BZh"}).

  Compressing with \code{type = "xz"} is equivalent to compressing a
  file with \command{xz -9e} (including adding the \sQuote{magic}
  header): decompression should cope with the contents of any file
  compressed with \command{xz} version 4.999 and some versions of
  \command{lzma}.  There are other versions, in particular \sQuote{raw}
  streams, that are not currently handled.

  All the types of compression can expand the input: for \code{"gzip"}
  and \code{"bzip"} the maximum expansion is known and so
  \code{memCompress} can always allocate sufficient space.  For
  \code{"xz"} it is possible (but extremely unlikely) that compression
  will fail if the output would have been too large.
}

\value{
  A raw vector or a character string (if \code{asChar = TRUE}).
}

\seealso{
  \link{connections}.

  \url{https://en.wikipedia.org/wiki/Data_compression} for background on
  data compression, \url{http://zlib.net/},
  \url{https://en.wikipedia.org/wiki/Gzip}, \url{http://www.bzip.org/},
  \url{https://en.wikipedia.org/wiki/Bzip2}, \url{http://tukaani.org/xz/}
  and \url{https://en.wikipedia.org/wiki/Xz} for references about the
  particular schemes used.
}

\examples{
txt <- readLines(file.path(R.home("doc"), "COPYING"))
sum(nchar(txt))
txt.gz <- memCompress(txt, "g")
length(txt.gz)
txt2 <- strsplit(memDecompress(txt.gz, "g", asChar = TRUE), "\n")[[1]]
stopifnot(identical(txt, txt2))
txt.bz2 <- memCompress(txt, "b")
length(txt.bz2)
## can auto-detect bzip2:
txt3 <- strsplit(memDecompress(txt.bz2, asChar = TRUE), "\n")[[1]]
stopifnot(identical(txt, txt3))

## xz compression is only worthwhile for large objects
txt.xz <- memCompress(txt, "x")
length(txt.xz)
txt3 <- strsplit(memDecompress(txt.xz, asChar = TRUE), "\n")[[1]]
stopifnot(identical(txt, txt3))
}

\keyword{file}
\keyword{connection}
