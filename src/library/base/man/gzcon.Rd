\name{gzcon}
\alias{gzcon}
\title{
  (De)compress I/O Through Connections
}
\description{
  \code{gzcon} provides a new connection that wraps an existing
  connection, and decompresses reads or compresses writes through that
  connection.  Standard \code{gzip} headers are assumed.
}
\usage{
gzcon(con, level = 6)
}
\arguments{
  \item{con}{a connection.}
  \item{level}{integer between 0 and 9, the compression level when writing.}
}
\details{
  If \code{con} is open then the new connection is opened.  Closing the
  wrapper connection will also close the underlying connection.
}
\value{
  An object inheriting from class \code{"connection"}.
}
\seealso{\code{\link{gzfile}}}
\examples{
\dontrun{## This example may not still be available
## print the value to see what objects were created.
con <- url("http://hesweb1.med.virginia.edu/biostat/s/data/sav/kprats.sav")
print(load(gzcon(con)))}

## gzfile and gzcon can inter-work.
## Of course here one would used gzfile, but file() can be replaced by
## any other connection generator.
zz <- gzfile("ex.gz", "w")
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
close(zz)
readLines(gzcon(file("ex.gz")))
unlink("ex.gz")

zz <- gzcon(file("ex.gz", "wb"))
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
close(zz)
readLines(gzfile("ex.gz"))
unlink("ex.gz")
}
\keyword{file}
\keyword{connection}
