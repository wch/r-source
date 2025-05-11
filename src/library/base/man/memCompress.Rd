% File src/library/base/man/memCompress.Rd
% Part of the R package, https://www.R-project.org
% Copyright 2009-2025 R Core Team
% Distributed under GPL 2 or later

\name{memCompress}
\alias{memCompress}
\alias{memDecompress}
\concept{\I{gzip}}
\concept{\I{bzip2}}
\concept{\I{lzma}}
\title{In-memory Compression and Decompression}
\description{
  In-memory compression or decompression for raw vectors.
}
\usage{
memCompress(from, type = c("gzip", "bzip2", "xz", "zstd", "none"))

memDecompress(from,
              type = c("unknown", "gzip", "bzip2", "xz", "zstd",
	               "none"), asChar = FALSE)
}
\arguments{
  \item{from}{raw vector.  For \code{memCompress}, a character vector
    will be converted to a raw vector with character strings separated
    by \code{"\n"}.  Types except \code{"bzip2"} support long
    raw vectors.
  }
  \item{type}{character string, the type of compression.  May be
    abbreviated to a single letter, defaults to the first of the alternatives.}
  \item{asChar}{logical: should the result be converted to a character
    string?  NB: character strings have a limit of
    \eqn{2^{31}-1}{2^31 - 1} bytes, so raw vectors should be used for
    large inputs.}
  }

\details{
  \code{type = "none"} passes the input through unchanged, but may be
  useful if \code{type} is a variable.

  \code{type = "unknown"} attempts to detect the type of compression
  applied (if any): this will always succeed for \command{bzip2}
  compression, and will succeed for other forms if there is a suitable
  header.  If no type of compression is detected this is the same as
  \code{type = "none"} but a warning is given.

  \command{gzip} compression uses whatever is the default compression
  level of the underlying library (usually \code{6}). This supports the
  RFC 1950 format, sometimes known as \sQuote{\I{zlib}} format, for
  compression and decompression and for decompression only RFC 1952, the
  \sQuote{\I{gzip}} format (which wraps the \sQuote{\I{zlib}} format with a
  header and footer).

  \command{bzip2} compression always adds a header (\code{"BZh"}).  The
  underlying library only supports in-memory (de)compression of up to
  \eqn{2^{31}-1}{2^31 - 1} elements.  Compression is equivalent to
  \command{bzip2 -9} (the default).

  \command{zstd} compression was introduced in \R 4.5.0: it is an
  optional part of the \R build and currently uses compression level 3
  which gives a good compression ratio vs compression speed trade-off.

  Compressing with \code{type = "xz"} is equivalent to compressing a
  file with \command{xz -9e} (including adding the \sQuote{magic}
  header): decompression should cope with the contents of any file
  compressed by \command{xz} version 4.999 and later, as well as by some
  versions of \command{lzma}.  There are other versions, in particular
  \sQuote{raw} streams, that are not currently handled.

  All the types of compression can expand the input: for \code{"gzip"}
  and \code{"bzip2"} the maximum expansion is known and so
  \code{memCompress} can always allocate sufficient space.  For
  \code{"xz"} it is possible (but extremely unlikely) that compression
  will fail if the output would have been too large.
}

\section{\code{libdeflate}}{
  Support for the \code{libdeflate} library was added for \R 4.4.0.  It
  uses different code for the RFC 1950 \sQuote{\I{zlib}} format (and RFC
  1952 for decompression), expected to be substantially faster than
  using the reference (or system) \I{zlib} library.  It is used for
  \code{type = "gzip"} if available.

  The headers and sources can be downloaded from
  \url{https://github.com/ebiggers/libdeflate} and pre-built versions
  are available for most Linux distributions.  It is used for binary
  Windows and macOS distributions.

  If it is used by an \R{} build and if so which version can be seen
  from \code{\link{extSoftVersion}()}.
}

\value{
  A raw vector or a character string (if \code{asChar = TRUE}).
}

\seealso{
  \link{connections}.

  \code{\link{extSoftVersion}} for the versions of the \code{zlib} or
  \code{libdeflate}, \code{bzip2} and \code{xz} libraries in use.

  \url{https://en.wikipedia.org/wiki/Data_compression} for background on
  data compression, \url{https://zlib.net/},
  \url{https://en.wikipedia.org/wiki/Gzip}, \url{http://www.bzip.org/},
  \url{https://en.wikipedia.org/wiki/Bzip2},
  %% \url{https://xz.tukaani.org/xz-utils/}
  and \url{https://en.wikipedia.org/wiki/XZ_Utils} for references about the
  particular schemes used.
}

\examples{
txt <- readLines(file.path(R.home("doc"), "COPYING"))
sum(nchar(txt))
txt.gz <- memCompress(txt, "g") # "gzip", the default
length(txt.gz)
txt2 <- strsplit(memDecompress(txt.gz, "g", asChar = TRUE), "\n")[[1]]
stopifnot(identical(txt, txt2))
## as from R 4.4.0 this is detected if not specified.
txt2b <- strsplit(memDecompress(txt.gz, asChar = TRUE), "\n")[[1]]
stopifnot(identical(txt2b, txt2))

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

## test decompressing a gzip-ed file
tf <- tempfile(fileext = ".gz")
con <- gzfile(tf, "w")
writeLines(txt, con)
close(con)
(nf <- file.size(tf))
# if (nzchar(Sys.which("file"))) system2("file", tf)
foo <- readBin(tf, "raw", n = nf)
unlink(tf)
## will detect the gzip header and choose type = "gzip"
txt3 <- strsplit(memDecompress(foo, asChar = TRUE), "\n")[[1]]
stopifnot(identical(txt, txt3))
}

\keyword{file}
\keyword{connection}
