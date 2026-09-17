% File src/library/base/man/gzcon.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
% Distributed under GPL 2 or later

\name{gzcon}
\alias{gzcon}
\title{
  (De)compress I/O Through Connections
}
\description{
  \code{gzcon} provides a modified connection that wraps an existing
  connection, and decompresses reads or compresses writes through that
  connection.  Standard \code{gzip} headers are assumed.
}
\usage{
gzcon(con, level = 6, allowNonCompressed = TRUE, text = FALSE)
}
\arguments{
  \item{con}{a connection.}
  \item{level}{integer between 0 and 9, the compression level when writing.}
  \item{allowNonCompressed}{logical.  When reading, should
    non-compressed input be allowed?}
  \item{text}{logical. Should the connection be text-oriented? This is
    distinct from the mode of the connection (must always be binary).
    If \code{TRUE}, \code{\link{pushBack}} works on the connection,
    otherwise \code{\link{readBin}} and friends apply.}
}
\details{
  If \code{con} is open then the modified connection is opened.  Closing
  the wrapper connection will also close the underlying connection.

  Reading from a connection which does not supply a \code{gzip} magic
  header is equivalent to reading from the original connection if
  \code{allowNonCompressed} is true, otherwise an error.

  Compressed output will contain embedded \abbr{NUL} bytes, and so \code{con}
  is not permitted to be a \code{\link{textConnection}} opened with
  \code{open = "w"}.  Use a writable \code{\link{rawConnection}} to
  compress data into a variable.  Flushing (\code{\link{flush}}) the
  connection will write out the already-compressed data and prepare to write
  a new compressed stream in case the connection is used again.

  The original connection becomes unusable: any object pointing to it will
  now refer to the modified connection.  For this reason, the new
  connection needs to be closed explicitly.
}
\value{
  An object inheriting from class \code{"connection"}.  This is the same
  connection \emph{number} as supplied, but with a modified internal
  structure.  It has binary mode.
}
\seealso{\code{\link{gzfile}}}
\examples{
\donttest{## Uncompress a data file from a URL
z <- gzcon(url("https://www.stats.ox.ac.uk/pub/datasets/csb/ch12.dat.gz"))
# read.table can only read from a text-mode connection.
raw <- textConnection(readLines(z))
close(z)
dat <- read.table(raw)
close(raw)
dat[1:4, ]
}

## gzfile and gzcon can inter-work.
## Of course here one would use gzfile, but file() can be replaced by
## any other connection generator.
zzfil <- tempfile(fileext = ".gz")
zz <- gzfile(zzfil, "w")
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
close(zz)
readLines(zz <- gzcon(file(zzfil, "rb")))
close(zz)
unlink(zzfil)

% Cygwin seems to require a different name %
zzfil2 <- tempfile(fileext = ".gz")
zz <- gzcon(file(zzfil2, "wb"))
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
s1 <- summary(zz); close(zz)
readLines(zz <- gzfile(zzfil2))
s. <- summary(zz); close(zz)
unlink(zzfil2)
stopifnot(identical(
    s.,
    list(description = zzfil2, class = "gzfile", mode = "rb",
         text = "text", opened = "closed", `can read` = "yes", `can write` = "no")))

## Since Sep 2026, PR#19172,  R can 
zz <- gzcon(zz2 <- rawConnection(raw(), "wb"))
writeLines("hello", zz); flush(zz)
raw <- rawConnectionValue(zz2)
str(sz <- summary(zz))
stopifnot(identical(sz, summary(zz2)))
close(zz) # also
zz <- gzcon(rawConnection(raw, "rb"))
readLines(zz) # "hello"
str(sz <- summary(zz)); close(zz)
zz2
}
\keyword{file}
\keyword{connection}
