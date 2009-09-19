% File src/library/base/man/memCompress.Rd
% Part of the R package, http://www.R-project.org
% Copyright 2009 R Core Development Team
% Distributed under GPL 2 or later

\name{memCompress}
\alias{memCompress}
\alias{memDecompress}
\title{In-memory Compression and Decompression}
\description{
  In-memory compression or decompression for raw vectors.
}
\usage{
memCompress(from, type = c("gzip", "bzip2", "xz", "none"))

memDecompress(from, type = c("gzip", "bzip2", "xz", "none"), asChar = FALSE)
}
\arguments{
  \item{from}{A raw vector.  For \code{memCompress} a character vector
    will be converted to a raw vector with elements separated by \code{"\n"}.}
  \item{type}{character string, the type of compression.  May be
    abbreviated to a single letter, defaults to the first of the alternatives.}
  \item{asChar}{logical: should the result be converted to a character
    string?}
}

\details{
  \code{type = "none"} passes the input through unchanged, but may be
  useful if \code{type} is a variable.

  For decompression it will auto-detect the \sQuote{magic} header
  (\code{"\x1f\x8b"}) added to files by the \command{gzip} program (and
  to files written by \code{\link{gzfile}}), but \code{memCompress} does
  not add such a header.

  \command{bzip2} compression always adds a header (\code{"BZh"}).
}

\note{
  \code{type = "xz"} may not be supported on a particular build of \R.
}

\value{
  A raw vector or a character string (if \code{asChar = TRUE}).
}

\seealso{
  \code{\link{connections}}.
}

\examples{
txt <- readLines(file.path(R.home("doc"), "AUTHORS"))
sum(nchar(txt))
txt.gz <- memCompress(txt, "g")
length(txt.gz)
txt2 <- strsplit(memDecompress(txt.gz, "g", TRUE), "\n")[[1]]
stopifnot(identical(txt, txt2))
txt.bz2 <- memCompress(txt, "b")
length(txt.bz2)
txt3 <- strsplit(memDecompress(txt.bz2, "b", TRUE), "\n")[[1]]
stopifnot(identical(txt, txt3))
}

\keyword{file}
\keyword{connection}
