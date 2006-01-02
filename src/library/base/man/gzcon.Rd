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
gzcon(con, level = 6, allowNonCompressed = TRUE)
}
\arguments{
  \item{con}{a connection.}
  \item{level}{integer between 0 and 9, the compression level when writing.}
  \item{allowNonCompressed}{logical.  When reading, should
    non-compressed files (lacking the \code{gzip} magic header) be allowed?}
}
\details{
  If \code{con} is open then the modified
  connection is opened.  Closing the
  wrapper connection will also close the underlying connection.

  Reading from a connection which does not supply a \code{gzip} magic
  header is equivalent to reading from the original connection if
  \code{allowNonCompressed} is true, otherwise an error.

  The original connection is unusable: any object pointing to it will
  now refer to the modified connection.
}
\value{
  An object inheriting from class \code{"connection"}.  This is the same
  connection \emph{number} as supplied, but with a modified internal structure.
}
\seealso{\code{\link{gzfile}}}
\examples{
\dontrun{## This example may not still be available
## print the value to see what objects were created.
con <- url("http://hesweb1.med.virginia.edu/biostat/s/data/sav/kprats.sav")
print(load(con))}

## gzfile and gzcon can inter-work.
## Of course here one would used gzfile, but file() can be replaced by
## any other connection generator.
zz <- gzfile("ex.gz", "w")
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
close(zz)
readLines(zz <- gzcon(file("ex.gz", "rb")))
close(zz)
unlink("ex.gz")

zz <- gzcon(file("ex.gz", "wb"))
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
close(zz)
readLines(zz <- gzfile("ex.gz"))
close(zz)
unlink("ex.gz")
}
\keyword{file}
\keyword{connection}
