\name{readChar}
\alias{readChar}
\alias{writeChar}
\title{Transfer Character Strings To and From Connections}
\description{
  Transfer character strings to and from connections, without assuming
  they are null-terminated on the connection.
}
\usage{
readChar(con, nchars)

writeChar(object, con,
          nchars = nchar(object, type="chars"), eos = "")
}
\arguments{
  \item{con}{A connection object or a character string naming a file.}
  \item{nchars}{integer, giving the lengths in characters of (unterminated)
    character strings to be read or written.}
  \item{object}{A character vector to be written to the connection.}
  \item{eos}{\sQuote{end of string}: character string .  The terminator
    to be written after each string, followed by an ASCII \code{nul};
    use \code{NULL} for no terminator at all.}
}
\details{
  These functions complement \code{\link{readBin}} and
  \code{\link{writeBin}} which read and write C-style zero-terminated
  character strings.  They are for strings of known length, and
  can optionally write an end-of-string mark.
  
  If \code{con} is a character string, the functions call
  \code{\link{file}} to obtain an file connection which is opened for
  the duration of the function call.

  If the connection is open it is read/written from its current
  position.  If it is not open, it is opened for the duration of the call
  and then closed again.  Connections can be open in either text or
  binary mode.

  Character strings containing ASCII \code{nul}(s) will be read correctly
  by and appear with embedded nuls in the character vector returned.
  
  If the character length requested for \code{readChar} is longer than
  the data available on the connection, what is available is
  returned.  For \code{writeChar} if too many characters are requested
  the output is zero-padded, with a warning.

  Missing strings are written as \code{NA}.
}

\value{
  For \code{readChar}, a character vector of length the number of
  items read (which might be less than \code{length(nchars)}).

  For \code{writeChar} none (strictly, invisible \code{NULL}).
}
\seealso{
  The \emph{R Data Import/Export} manual.

  \code{\link{connections}}, \code{\link{readLines}},
  \code{\link{writeLines}}, \code{\link{readBin}}
}
\examples{
## test fixed-length strings
zz <- file("testchar", "wb")
x <- c("a", "this will be truncated", "abc")
nc <- c(3, 10, 3)
writeChar(x, zz, nc, eos=NULL)
writeChar(x, zz, eos="\r\n")
close(zz)

zz <- file("testchar", "rb")
readChar(zz, nc)
readChar(zz, nchar(x)+3) # need to read the terminator explicitly
close(zz)
unlink("testbin")
}
\keyword{file}
\keyword{connection}
