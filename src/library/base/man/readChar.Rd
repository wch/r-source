% File src/library/base/man/readChar.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{readChar}
\alias{readChar}
\alias{writeChar}
\title{Transfer Character Strings To and From Connections}
\description{
  Transfer character strings to and from connections, without assuming
  they are null-terminated on the connection.
}
\usage{
readChar(con, nchars, useBytes = FALSE)

writeChar(object, con, nchars = nchar(object, type = "chars"),
          eos = "", useBytes = FALSE)
}
\arguments{
  \item{con}{A \link{connection} object, or a character string naming a file,
    or a raw vector.}

  \item{nchars}{integer vector, giving the lengths in characters of
    (unterminated) character strings to be read or written.  Elements
    must be >= 0 and not \code{NA}.}

  \item{useBytes}{logical: For \code{readChar}, should \code{nchars} be
    regarded as a number of bytes not characters in a multi-byte
    locale?  For \code{writeChar}, see \code{\link{writeLines}}.}

  \item{object}{A character vector to be written to the connection, at
    least as long as \code{nchars}.}

  \item{eos}{\sQuote{end of string}: character string .  The terminator
    to be written after each string, followed by an ASCII \code{nul};
    use \code{NULL} for no terminator at all.}
}
\details{
  These functions complement \code{\link{readBin}} and
  \code{\link{writeBin}} which read and write C-style zero-terminated
  character strings.  They are for strings of known length, and
  can optionally write an end-of-string mark.  They are intended only
  for character strings valid in the current locale.

  These functions are intended to be used with binary-mode connections.
  If \code{con} is a character string, the functions call
  \code{\link{file}} to obtain a binary-mode file connection which is
  opened for the duration of the function call.

  If the connection is open it is read/written from its current
  position.  If it is not open, it is opened for the duration of the
  call in an appropriate mode (binary read or write) and then closed
  again.  An open connection must be in binary mode.

  If \code{readChar} is called with \code{con} a raw vector, the data in
  the vector is used as input.  If \code{writeChar} is called with
  \code{con} a raw vector, it is just an indication that a raw vector
  should be returned.

  Character strings containing ASCII \code{nul}(s) will be read
  correctly by \code{readChar} but truncated at the first
  \code{nul} with a warning.

  If the character length requested for \code{readChar} is longer than
  the data available on the connection, what is available is
  returned.  For \code{writeChar} if too many characters are requested
  the output is zero-padded, with a warning.

  Missing strings are written as \code{NA}.
}

\value{
  For \code{readChar}, a character vector of length the number of
  items read (which might be less than \code{length(nchars)}).

  For \code{writeChar}, a raw vector (if \code{con} is a raw vector) or
  invisibly \code{NULL}.
}

\note{
  Earlier versions of \R allowed embedded nul bytes within character
  strings, but not \R >= 2.8.0.  \code{readChar} was commonly used to
  read fixed-size zero-padded byte fields for which \code{readBin} was
  unsuitable.  \code{readChar} can still be used for such fields if
  there are no embedded nuls: otherwise \code{readBin(what = "raw")}
  provides an alternative.

  \code{nchars} will be interpreted in bytes not characters in a
  non-UTF-8 multi-byte locale, with a warning.

  There is little validity checking of UTF-8 reads.

  Using these functions on a text-mode connection may work but should
  not be mixed with text-mode access to the connection, especially if
  the connection was opened with an \code{encoding} argument.
}

\seealso{
  The \sQuote{R Data Import/Export} manual.

  \code{\link{connections}}, \code{\link{readLines}},
  \code{\link{writeLines}}, \code{\link{readBin}}
}
\examples{
## test fixed-length strings
zz <- file("testchar", "wb")
x <- c("a", "this will be truncated", "abc")
nc <- c(3, 10, 3)
writeChar(x, zz, nc, eos = NULL)
writeChar(x, zz, eos = "\r\n")
close(zz)

zz <- file("testchar", "rb")
readChar(zz, nc)
readChar(zz, nchar(x)+3) # need to read the terminator explicitly
close(zz)
unlink("testchar")
}
\keyword{file}
\keyword{connection}
