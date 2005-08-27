\name{readBin}
\alias{readBin}
\alias{writeBin}
\alias{readChar}
\alias{writeChar}
\title{Transfer Binary Data To and From Connections}
\description{
  Read binary data from a connection, or write binary data to a connection.
}
\usage{
readBin(con, what, n = 1, size = NA, signed = TRUE,
        endian = .Platform$endian)
writeBin(object, con, size = NA, endian = .Platform$endian)

readChar(con, nchars)
writeChar(object, con, nchars = nchar(object, type="chars"), eos = "")
}
\arguments{
  \item{con}{A connection object or a character string naming a file or
    a raw vector.}
  \item{what}{Either an object whose mode will give the mode of the
    vector to be read, or a character vector of length one describing
    the mode: one of \code{"numeric", "double", "integer", "int",
      "logical", "complex", "character", "raw"}.}
  \item{n}{integer.  The (maximal) number of records to be
    read.  You can use an over-estimate here, but not too large as
    storage is reserved for \code{n} items.}
  \item{size}{integer.  The number of bytes per element in the byte
    stream.  The default, \code{NA}, uses the natural size.  Size changing
    is not supported for raw and complex vectors.}
  \item{signed}{logical. Only used for integers of sizes 1 and 2,
    when it determines if the quantity on file
    should be regarded as a signed or unsigned integer.}
  \item{endian}{The endian-ness (\code{"big"} or \code{"little"} of the
    target system for the file.  Using \code{"swap"} will force swapping
    endian-ness.}
  \item{object}{An \R object to be written to the connection.}
  \item{nchars}{integer, giving the lengths in characters of (unterminated)
    character strings to be read or written.}
  \item{eos}{character. The terminator to be written after each string,
    followed by an ASCII \code{nul}; use \code{NULL} for no terminator
    at all.}
}
\details{
  If the \code{con} is a character string, the functions call
  \code{\link{file}} to obtain an file connection which is opened for
  the duration of the function call.

  If the connection is open it is read/written from its current position.
  If it is not open, it is opened for the duration of the call and then
  closed again.

  If \code{readBin} is called \code{con} with a raw vector, the data in
  the vector is used as input.  If \code{writeBin} is called with
  \code{con} a raw vector, it is just an indication that a raw vector
  should be returned.
  
  If \code{size} is specified and not the natural size of the object,
  each element of the vector is coerced to an appropriate type before
  being written or as it is read.  Possible sizes are 1, 2, 4 and
  possibly 8 for integer or logical vectors, and 4, 8 and possibly 12/16
  for numeric vectors.  (Note that coercion occurs as signed types
  except if \code{signed = FALSE} when reading integers of sizes 1 and 2.)
  Changing sizes is unlikely to preserve \code{NA}s, and the extended
  precision sizes are unlikely to be portable across platforms.

  \code{readBin} and \code{writeBin} read and write C-style
  zero-terminated character strings.  Input strings are limited to 10000
  characters.  \code{readChar} and \code{writeChar} allow more
  flexibility, and can also be used on text-mode connections.

  Handling \R's missing and special (\code{Inf}, \code{-Inf} and
  \code{NaN}) values is discussed in the \emph{R Data Import/Export} manual.
}

\note{
  Integer read/writes of size 8 will be available if either C type
  \code{long} is of size 8 bytes or C type \code{long long} exists and
  is of size 8 bytes.

  Real read/writes of size \code{sizeof(long double)} (usually 12 or 16
  bytes) will be available only if that type is available and different
  from \code{double}.

  Character strings containing ASCII \code{nul} will be read correctly
  by \code{readChar} and appear as embedded nuls in the character vector
  returned.
  
  If the character length requested for \code{readChar} is longer than
  the data available on the connection, what is available is returned.
  For \code{writeChar} if too many characters are requested the output
  is zero-padded, with a warning.

  If \code{readBin(what = character())} is used incorrectly on a file
  which does not contain C-style character strings, warnings (usually
  many) are given.  From a connection, the input will be broken into
  pieces of length 10000 with any final part being discarded.
}

\value{
  For \code{readBin}, a vector of appropriate mode and length the number of
  items read (which might be less than \code{n}).

  For \code{readChar}, a character vector of length the number of
  items read (which might be less than \code{length(nchars)}).

  For \code{writeBin}, a raw vector (if \code{con} is a raw vector) or
  invisibly \code{NULL}.
  
  For \code{writeChar} none (strictly, invisible \code{NULL}).
}
\seealso{
  The \emph{R Data Import/Export} manual.

  \code{\link{connections}}, \code{\link{readLines}},
  \code{\link{writeLines}}.

  \code{\link{.Machine}} for the sizes of \code{long}, \code{long long}
  and \code{long double}.
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
if(.Machine$sizeof.long == 8 || .Machine$sizeof.longlong == 8)
    writeBin(as.integer(5^(1:10)), zz, size = 8)
if((s <-.Machine$sizeof.longdouble) > 8)
    writeBin((pi/3)^(1:10), zz, size = s)
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
if(.Machine$sizeof.long == 8 || .Machine$sizeof.longlong == 8)
    readBin(zz, integer(), 10,  size = 8)
if((s <-.Machine$sizeof.longdouble) > 8)
    readBin(zz, numeric(), 10, size = s)
close(zz)
unlink("testbin")
stopifnot(z2 == z)

## test fixed-length strings
zz <- file("testbin", "wb")
x <- c("a", "this will be truncated", "abc")
nc <- c(3, 10, 3)
writeChar(x, zz, nc, eos=NULL)
writeChar(x, zz, eos="\r\n")
close(zz)

zz <- file("testbin", "rb")
readChar(zz, nc)
readChar(zz, nchar(x)+3) # need to read the terminator explicitly
close(zz)
unlink("testbin")

## signed vs unsigned ints
zz <- file("testbin", "wb")
x <- as.integer(seq(0, 255, 32))
writeBin(x, zz, size=1)
writeBin(x, zz, size=1)
x <- as.integer(seq(0, 60000, 10000))
writeBin(x, zz, size=2)
writeBin(x, zz, size=2)
close(zz)
zz <- file("testbin", "rb")
readBin(zz, integer(), 8, size=1)
readBin(zz, integer(), 8, size=1, signed=FALSE)
readBin(zz, integer(), 7, size=2)
readBin(zz, integer(), 7, size=2, signed=FALSE)
close(zz)
unlink("testbin")

## use of raw
z <- writeBin(pi^{1:5}, raw(), size = 4)
readBin(z, numeric(), 5, size = 4)
z <- writeBin(c("a", "test", "of", "character"), raw())
rawToChar(z)
readBin(z, character(), 4)
}
\keyword{file}
\keyword{connection}
