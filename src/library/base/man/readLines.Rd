% File src/library/base/man/readLines.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{readLines}
\alias{readLines}
\title{Read Text Lines from a Connection}
\description{
  Read some or all text lines from a connection.
}
\usage{
readLines(con = stdin(), n = -1L, ok = TRUE, warn = TRUE,
          encoding = "unknown", skipNul = FALSE)
}
\arguments{
  \item{con}{a \link{connection} object or a character string.}
  \item{n}{integer.  The (maximal) number of lines to
    read.  Negative values indicate that one should read up to the end of
    input on the connection.}
  \item{ok}{logical.  Is it OK to reach the end of the connection before
    \code{n > 0} lines are read?  If not, an error will be generated.}
  \item{warn}{logical.  Warn if a text file is missing a final EOL or if
    there are embedded nuls in the file.}
  \item{encoding}{encoding to be assumed for input strings.  It is
    used to mark character strings as known to be in
    Latin-1 or UTF-8: it is not used to re-encode the input.  To do the
    latter, specify the encoding as part of the connection \code{con} or
    via \code{\link{options}(encoding=)}: see the examples.
#ifdef windows
    See also \sQuote{Details}.
#endif
  }
  \item{skipNul}{logical: should nuls be skipped?}
}
\details{
  If the \code{con} is a character string, the function calls
  \code{\link{file}} to obtain a file connection which is opened for
  the duration of the function call.  This can be a compressed file.

  If the connection is open it is read from its current position.  If it
  is not open, it is opened in \code{"rt"} mode for the duration of
  the call and then closed (but not destroyed; one must call
  \code{\link{close}} to do that).

  If the final line is incomplete (no final EOL marker) the behaviour
  depends on whether the connection is blocking or not.  For a
  non-blocking text-mode connection the incomplete line is pushed back,
  silently.  For all other connections the line will be accepted, with a
  warning.

  Whatever mode the connection is opened in, any of LF, CRLF or CR will
  be accepted as the EOL marker for a line.

  Embedded nuls in the input stream will terminate the line currently
  being read, with a warning (unless \code{skipNul = TRUE} or \code{warn
    = FALSE}).

  If \code{con} is a not-already-open \link{connection} with a non-default
  \code{encoding} argument, the text is converted to UTF-8 and declared
  as such (and the \code{encoding} argument to \code{readLines} is ignored).
  See the examples.
}
\value{
  A character vector of length the number of lines read.

  The elements of the result have a declared encoding if \code{encoding} is
  \code{"latin1"} or \code{"UTF-8"},
}
\note{
  The default connection, \code{\link{stdin}}, may be different from
  \code{con = "stdin"}: see \code{\link{file}}.
}
\seealso{
  \code{\link{connections}}, \code{\link{writeLines}}, \code{\link{readBin}},
  \code{\link{scan}}}

\examples{
fil <- tempfile(fileext = ".data")
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = fil,
    sep = "\n")
readLines(fil, n = -1)
unlink(fil) # tidy up

## difference in blocking
fil <- tempfile("test")
cat("123\nabc", file = fil)
readLines(fil) # line with a warning

con <- file(fil, "r", blocking = FALSE)
readLines(con) # empty
cat(" def\n", file = fil, append = TRUE)
readLines(con) # gets both
close(con)

unlink(fil) # tidy up

\dontrun{
# read a 'Windows Unicode' file
A <- readLines(con <- file("Unicode.txt", encoding = "UCS-2LE"))
close(con)
unique(Encoding(A)) # will most likely be UTF-8
}}
\keyword{file}
\keyword{connection}
