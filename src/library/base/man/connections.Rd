\name{connections}
\alias{connections}
\alias{connection}
\alias{file}
\alias{pipe}
\alias{fifo}
\alias{gzfile}
\alias{unz}
\alias{bzfile}
\alias{url}
\alias{socketConnection}
\alias{open}
\alias{open.connection}
\alias{isOpen}
\alias{isIncomplete}
\alias{close}
\alias{close.connection}
\alias{flush}
\alias{flush.connection}
\alias{print.connection}
\alias{summary.connection}
\alias{ISOLatin1}
\alias{MacRoman}
\alias{WinAnsi}
\alias{native.enc}
\concept{encoding}
\concept{compression}
\title{Functions to Manipulate Connections}
\description{
  Functions to create, open and close connections.
}
\usage{
file(description = "", open = "", blocking = TRUE,
     encoding = getOption("encoding"))
pipe(description, open = "", encoding = getOption("encoding"))
fifo(description = "", open = "", blocking = FALSE,
     encoding = getOption("encoding"))
gzfile(description, open = "", encoding = getOption("encoding"),
       compression = 6)
unz(description, filename, open = "", encoding = getOption("encoding"))
bzfile(description, open = "", encoding = getOption("encoding"))
url(description, open = "", blocking = TRUE,
    encoding = getOption("encoding"))
socketConnection(host = "localhost", port, server = FALSE,
                 blocking = FALSE, open = "a+",
                 encoding = getOption("encoding"))

open(con, \dots)
\method{open}{connection}(con, open = "r", blocking = TRUE, \dots)

close(con, \dots)
\method{close}{connection}(con, type = "rw", \dots)

flush(con)

isOpen(con, rw = "")
isIncomplete(con)
}
\arguments{
  \item{description}{character. A description of the connection. For
    \code{file} and \code{pipe} this is a path to the file to be opened.
    For \code{url} it is a complete URL, including schemes
    (\code{http://}, \code{ftp://} or \code{file://}).  \code{file} also
    accepts complete URLs.
  }
  \item{filename}{a filename within a zip file.}
  \item{con}{a connection.}
  \item{host}{character.  Host name for port.}
  \item{port}{integer.  The TCP port number.}
  \item{server}{logical.  Should the socket be a client or a server?}
  \item{open}{character.  A description of how to open the connection
    (if at all). See Details for possible values.}
  \item{blocking}{logical.  See \sQuote{Blocking} section below.}
  \item{encoding}{An integer vector of length 256.}
  \item{compression}{integer in 0--9.  The amount of compression to be
    applied when writing, from none to maximal.  The default is a good
    space/time compromise.}
  \item{type}{character. Currently ignored.}
  \item{rw}{character.  Empty or \code{"read"} or \code{"write"},
    partial matches allowed.}
  \item{\dots}{arguments passed to or from other methods.}
}
\details{
  The first eight functions create connections.  By default the
  connection is not opened (except for \code{socketConnection}), but may
  be opened by setting a non-empty value of argument \code{open}.

  \code{gzfile} applies to files compressed by \file{gzip}, and
  \code{bzfile} to those compressed by \file{bzip2}: such connections can
  only be binary.

  \code{unz} reads (only) single files within zip files, in binary mode.
  The description is the full path, with \file{.zip} extension if required.

  All platforms support \code{file}, \code{gzfile}, \code{bzfile},
  \code{unz} and \code{url("file://")} connections.  The other types may
  be partially implemented or not implemented at all.  (They do work on
  most Unix platforms, and all but \code{fifo} on Windows.)

  Proxies can be specified for \code{url} connections: see
  \code{\link{download.file}}.

  \code{open}, \code{close} and \code{seek} are generic functions: the
  following applies to the methods relevant to connections.

  \code{open} opens a connection.  In general functions using
  connections will open them if they are not open, but then close them
  again, so to leave a connection open call \code{open} explicitly.

  Possible values for the mode \code{open} to open a connection are
  \describe{
    \item{\code{"r"} or \code{"rt"}}{Open for reading in text mode.}
    \item{\code{"w"} or \code{"wt"}}{Open for writing in text mode.}
    \item{\code{"a"} or \code{"at"}}{Open for appending in text mode.}
    \item{\code{"rb"}}{Open for reading in binary mode.}
    \item{\code{"wb"}}{Open for writing in binary mode.}
    \item{\code{"ab"}}{Open for appending in binary mode.}
    \item{\code{"r+"}, \code{"r+b"}}{Open for reading and writing.}
    \item{\code{"w+"}, \code{"w+b"}}{Open for reading and writing,
      truncating file initially.}
    \item{\code{"a+"}, \code{"a+b"}}{Open for reading and appending.}
  }
  Not all modes are applicable to all connections: for example URLs can
  only be opened for reading.  Only file and socket connections can be
  opened for reading and writing/appending.
  For many connections there is little or no
  difference between text and binary modes, but there is for file-like
  connections on Windows, and \code{\link{pushBack}} is text-oriented and
  is only allowed on connections open for reading in text mode.

  \code{close} closes and destroys a connection.

  \code{flush} flushes the output stream of a connection open for
  write/append (where implemented).

  If for a \code{file} connection the description is \code{""}, the file
  is immediately opened in \code{"w+"} mode and unlinked from the file
  system.  This provides a temporary file to write to and then read from.
  
  The encoding vector is used to map the input from a file or pipe to
  the platform's native character set.  Supplied examples are
  \code{native.enc} as well as \code{MacRoman}, \code{WinAnsi} and
  \code{ISOLatin1}, whose actual encoding is platform-dependent.  Missing
  characters are mapped to a space in these encodings.
}
\value{
  \code{file}, \code{pipe}, \code{fifo}, \code{url}, \code{gzfile} and
  \code{socketConnection} return a connection object
  which inherits from class \code{"connection"} and has a first more
  specific class.

  \code{isOpen} returns a logical value, whether the connection is
  currently open.

  \code{isIncomplete} returns a logical value, whether last read attempt
  was blocked, or for an output text connection whether there is
  unflushed output.
}
\section{Blocking}{
  The default condition for all but fifo and socket connections is to be in
  blocking mode.  In that mode, functions do not return to the \R
  evaluator until they are complete.   In non-blocking mode, operations
  return as soon as possible, so on input they will return with whatever
  input is available (possibly none) and for output they will return
  whether or not the write succeeded.

  The function \code{\link{readLines}} behaves differently in respect of
  incomplete last lines in the two modes: see its help page.

  Even when a connection is in blocking mode, attempts are made to
  ensure that it does not block the event loop and hence the operation
  of GUI parts of \R.  These do not always succeed, and the whole
  process will be blocked during a DNS lookup on Unix, for example.

  Most blocking operations on URLs and sockets are subject to the
  timeout set by \code{options("timeout")}.  Note that this is a timeout
  for no response at all, not for the whole operation.
}
\section{Fifos}{
  Fifos default to non-blocking.  That follows Svr4 and it probably most
  natural, but it does have some implications.
  In particular, opening a non-blocking fifo connection for writing
  (only) will fail unless some other process is reading on the fifo.

  Opening a fifo for both reading and writing (in any mode: one can only
  append to fifos) connects both sides of the fifo to the \R process,
  and provides an similar facility to \code{file()}.
}
#ifdef windows
\section{Clipboard}{
  Under Windows, \code{file} can also be used with \code{description =
    "clipboard"} in modes \code{"r"} and \code{"w"} only.
  
  When the clipboard is opened for reading, the contents are immediately
  copied to internal storage in the connection.

  When writing to the clipboard, the output is
  copied to the clipboard only when the connection is closed or flushed.
  There is a 32Kb limit on the text to be written to the clipboard.
}
#endif
\note{
  \R's connections are modelled on those in S version 4 (see Chambers,
  1998).  However \R goes well beyond the Svr4 model, for example in
  output text connections and URL, gzfile, bzfile and socket
  connections.

  The default mode in \R is \code{"r"} except for socket connections.
  This differs from Svr4, where it is the equivalent of \code{"r+"},
  known as \code{"*"}.

  On platforms where \code{vsnprintf} does not return the needed length
  of output (e.g., Windows) there is a 100,000 character output limit on
  the length of line for \code{fifo}, \code{gzfile} and \code{bzfile}
  connections: longer lines will be truncated with a warning.
}
\references{
  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language.} Springer.
}

\seealso{
  \code{\link{textConnection}}, \code{\link{seek}}, \code{\link{readLines}},
  \code{\link{readBin}}, \code{\link{writeLines}}, \code{\link{writeBin}},
  \code{\link{showConnections}}, \code{\link{pushBack}}.

  \code{\link{capabilities}} to see if \code{gzfile}, \code{url},
  \code{fifo} and \code{socketConnection} are supported by this build of \R.

#ifdef windows
  To flush output to the console, see \code{\link{flush.console}}.
#endif
}

\examples{
zz <- file("ex.data", "w")  # open an output file connection
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
cat("One more line\n", file = zz)
close(zz)
readLines("ex.data")
unlink("ex.data")

zz <- gzfile("ex.gz", "w")  # compressed file
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
close(zz)
readLines(gzfile("ex.gz"))
unlink("ex.gz")

zz <- bzfile("ex.bz2", "w")  # bzip2-ed file
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
close(zz)
print(readLines(bzfile("ex.bz2")))
unlink("ex.bz2")

## An example of a file open for reading and writing
Tfile <- file("test1", "w+")
c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
cat("abc\ndef\n", file=Tfile)
readLines(Tfile)
seek(Tfile, 0, rw="r") # reset to beginning
readLines(Tfile)
cat("ghi\n", file=Tfile)
readLines(Tfile)
close(Tfile)
unlink("test1")

## We can do the same thing with an anonymous file.
Tfile <- file()
cat("abc\ndef\n", file=Tfile)
readLines(Tfile)
close(Tfile)

if(capabilities("fifo")) {
  zz <- fifo("foo", "w+")
  writeLines("abc", zz)
  print(readLines(zz))
  close(zz)
  unlink("foo")
}

\dontrun{## Unix examples of use of pipes

# read listing of current directory
readLines(pipe("ls -1"))

# remove trailing commas. Suppose
\% cat data2
450, 390, 467, 654,  30, 542, 334, 432, 421,
357, 497, 493, 550, 549, 467, 575, 578, 342,
446, 547, 534, 495, 979, 479
# Then read this by
scan(pipe("sed -e s/,$// data2"), sep=",")

# convert decimal point to comma in output
# both R strings and (probably) the shell need \ doubled
zz <- pipe(paste("sed s/\\\\\\\\./,/ >", "outfile"), "w")
cat(format(round(rnorm(100), 4)), sep = "\n", file = zz)
close(zz)
file.show("outfile", delete.file=TRUE)}

\dontrun{## example for Unix machine running a finger daemon

con <- socketConnection(port = 79, blocking = TRUE)
writeLines(paste(system("whoami", intern=TRUE), "\r", sep=""), con)
gsub(" *$", "", readLines(con))
close(con)}

\dontrun{## two R processes communicating via non-blocking sockets
# R process 1
con1 <- socketConnection(port = 6011, server=TRUE)
writeLines(LETTERS, con1)
close(con1)

# R process 2
con2 <- socketConnection(Sys.info()["nodename"], port = 6011)
# as non-blocking, may need to loop for input
readLines(con2)
while(isIncomplete(con2)) {Sys.sleep(1); readLines(con2)}
close(con2)}
}
\keyword{file}
\keyword{connection}
