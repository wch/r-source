\name{readBin}
\alias{readBin}
\alias{writeBin}
\title{Transfer Binary Data To and From Connections}
\description{
  Read binary data from a connection, or write binary data to a connection.
}
\usage{
readBin(con, what, n = -1, size = NA, endian = .Platform$endian)
writeBin(object, con, size = NA, endian = .Platform$endian)
}
\arguments{
  \item{con}{A connection object or a character string.}
  \item{what}{Either an object whose mode will give the mode of the
    vector to be read, or a character vector of length one describing
    the mode: one of \code{"numeric", "double", "integer", "int",
      "logical", "complex", "character"}.}
  \item{n}{integer.  The (maximal) number of records to be
    read. Negative values indicate that one should read up to the end of
    the connection.}
  \item{size}{integer.  The number of bytes per element in the byte
    stream.  The default, \code{NA}, uses the natural size.  Size changing
    is not supported for complex vectors.}
  \item{endian}{The endian-ness (\code{"big"} or \code{"little"} of the
    target system for the file.  Using \code{"swap"} will force swapping
    endian-ness.}
  \item{object}{An \R object to be written to the connection.}
}
\details{
  If the \code{con} is a character string, the functions call
  \code{\link{file}} to obtain an file connection which is opened for
  the duration of the function call.

  If the connection is open it is read/written from its current position.
  If it is not open, it is opened for the duration of the call and then
  closed again.

  If \code{size} is specified and not the natural size of the object,
  each element of the vector is coerced to an appropriate type before
  being written or as it is read.  Possible sizes are 1, 2, 4 and
  possibly 8 for integer or logical vectors, and 4, 8 and possibly 12/16
  for numeric vectors.  (Note that coercion occurs as signed types.)
  Changing sizes is unlikely to preserve \code{NA}s, and the extended
  precision sizes are unlikely to be portable across platforms.
}
\note{
  Integer read/writes of size 8 will be available if either C type
  \code{long} is of size 8 bytes or C type \code{long long} exists and
  is of size 8 bytes.

  Real read/writes of size \code{sizeof(long double)} (usually 12 or 16
  bytes) will be available only if that type is available and different
  from \code{double}.
}
\value{
  For \code{readBin}, a vector of appropriate mode and the number of
  items read (which might be less than \code{m}).

  For \code{writeBin}, none.
}
\seealso{
  \code{\link{connection}}, \code{\link{readLines}}, \code{\link{writeLines}}
}
\examples{
zz <- file("testbin", "wb")
writeBin(1:10, zz)
writeBin(pi, zz, endian="swap")
writeBin(pi, zz, size=4)
writeBin(pi^2, zz, size=4, endian="swap")
writeBin(pi+3i, zz)
writeBin("A test of a connection", zz)
z <- paste("A very long string", 1:100, collapse=" + ")
writeBin(z, zz)
close(zz)

zz <- file("testbin", "rb")
readBin(zz, integer(), 4)
readBin(zz, integer(), 6)
readBin(zz, numeric(), 1, endian="swap")
readBin(zz, numeric(), size=4)
readBin(zz, numeric(), size=4, endian="swap")
readBin(zz, complex(), 1)
readBin(zz, character(), 1)
z2 <- readBin(zz, character(), 1)
close(zz)
unlink("testbin")
stopifnot(z2 == z)
}
\keyword{file}
\keyword{connection}
