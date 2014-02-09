% File src/library/base/man/connections.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{connections}
\alias{connections}
\alias{connection}
\alias{file}
\alias{clipboard}
\alias{pipe}
\alias{fifo}
\alias{gzfile}
\alias{unz}
\alias{bzfile}
\alias{xzfile}
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
\concept{encoding}
\concept{compression}
\concept{decompression}
\concept{zip}
\concept{gzip}
\concept{bzip2}
\concept{lzma}
\title{Functions to Manipulate Connections}
\description{
  Functions to create, open and close connections.
}
\usage{
file(description = "", open = "", blocking = TRUE,
     encoding = getOption("encoding"), raw = FALSE)

url(description, open = "", blocking = TRUE,
    encoding = getOption("encoding"))

gzfile(description, open = "", encoding = getOption("encoding"),
       compression = 6)

bzfile(description, open = "", encoding = getOption("encoding"),
       compression = 9)

xzfile(description, open = "", encoding = getOption("encoding"),
       compression = 6)

unz(description, filename, open = "",
    encoding = getOption("encoding"))

pipe(description, open = "", encoding = getOption("encoding"))

fifo(description, open = "", blocking = FALSE,
     encoding = getOption("encoding"))

socketConnection(host = "localhost", port, server = FALSE,
                 blocking = FALSE, open = "a+",
                 encoding = getOption("encoding"),
                 timeout = getOption("timeout"))

open(con, \dots)
\method{open}{connection}(con, open = "r", blocking = TRUE, \dots)

close(con, \dots)
\method{close}{connection}(con, type = "rw", \dots)

flush(con)

isOpen(con, rw = "")
isIncomplete(con)
}
\arguments{
  \item{description}{character string.  A description of the connection:
    see \sQuote{Details}.}
  \item{open}{character.  A description of how to open the connection
    (if it should be opened initially).  See section \sQuote{Modes} for
    possible values.}
  \item{blocking}{logical.  See the \sQuote{Blocking} section.}
  \item{encoding}{The name of the encoding to be used.  See the
    \sQuote{Encoding} section.}
  \item{raw}{logical.  If true, a \sQuote{raw} interface is used which
    will be more suitable for arguments which are not regular files,
    e.g. character devices.  This suppresses the check for a compressed
    file when opening for text-mode reading, and asserts that the
    \sQuote{file} may not be seekable.}
  \item{compression}{integer in 0--9.  The amount of compression to be
    applied when writing, from none to maximal available.  For
    \code{xzfile} can also be negative: see the \sQuote{Compression}
    section.}
  \item{timeout}{numeric: the timeout (in seconds) to be used for this
    connection.  Beware that some OSes may treat very large values as
    zero: however the POSIX standard requires values up to 31 days to be
    supported.}
  \item{filename}{a filename within a zip file.}
  \item{host}{character.  Host name for port.}
  \item{port}{integer.  The TCP port number.}
  \item{server}{logical.  Should the socket be a client or a server?}
  \item{con}{a connection.}
  \item{type}{character.  Currently ignored.}
  \item{rw}{character.  Empty or \code{"read"} or \code{"write"},
    partial matches allowed.}
  \item{\dots}{arguments passed to or from other methods.}
}
\details{
  The first nine functions create connections.  By default the
  connection is not opened (except for \code{socketConnection}), but may
  be opened by setting a non-empty value of argument \code{open}.

  For \code{file} the description is a path to the file to be opened or
  a complete URL (when it is the same as calling \code{url}), or
  \code{""} (the default) or \code{"clipboard"} (see the
  \sQuote{Clipboard} section).  Use \code{"stdin"} to refer to the
  C-level \sQuote{standard input} of the process (which need not be
  connected to anything in a console or embedded version of \R, and is
  not in \code{RGui} on Windows).  See also \code{\link{stdin}()} for
  the subtly different R-level concept of \code{stdin}.

  For \code{url} the description is a complete URL, including scheme
  (such as \samp{http://}, \samp{ftp://} or \samp{file://}).  Proxies
  can be specified for HTTP and FTP \code{url} connections: see
  \code{\link{download.file}}.

  For \code{gzfile} the description is the path to a file compressed by
  \command{gzip}: it can also open for reading uncompressed files and
  (as from \R 2.10.0) those compressed by \command{bzip2}, \command{xz}
  or \command{lzma}.

  For \code{bzfile} the description is the path to a file compressed by
  \command{bzip2}.

  For \code{xzfile} the description is the path to a file compressed by
  \command{xz} (\url{http://en.wikipedia.org/wiki/Xz}) or (for reading
  only) \command{lzma} (\url{http://en.wikipedia.org/wiki/LZMA}).

  \code{unz} reads (only) single files within zip files, in binary mode.
  The description is the full path to the zip file, with \file{.zip}
  extension if required.

  For \code{pipe} the description is the command line to be piped to or
  from.  This is run in a shell, on Windows that specified by the
  \env{COMSPEC} environment variable.

  For \code{fifo} the description is the path of the fifo.  (Windows
  does not have fifos, so attempts to use this function there are an
  error.  It was possible to use \code{file} with fifos prior to \R
  2.10.0, but \code{raw = TRUE} is now required for reading, and
  \code{fifo} was always the documented interface.) % PR#14366

  All platforms support \code{file}, \code{pipe}, \code{gzfile},
  \code{bzfile}, \code{xzfile}, \code{unz} and \code{url("file://")}
  connections.  The other connections may be partially implemented or
  not implemented at all.  (They do work on most Unix platforms, and all
  but \code{fifo} on Windows.)

  The intention is that \code{file} and \code{gzfile} can be used
  generally for text input (from files and URLs) and binary input
  respectively.

  \code{open}, \code{close} and \code{seek} are generic functions: the
  following applies to the methods relevant to connections.

  \code{open} opens a connection.  In general functions using
  connections will open them if they are not open, but then close them
  again, so to leave a connection open call \code{open} explicitly.

  \code{close} closes and destroys a connection.  This will happen
  automatically in due course (with a warning) if there is no longer an
  \R object referring to the connection.

  A maximum of 128 connections can be allocated (not necessarily open)
  at any one time.  Three of these are pre-allocated (see
  \code{\link{stdout}}).   The OS will impose limits on the numbers of
  connections of various types, but these are usually larger than 125.

  \code{flush} flushes the output stream of a connection open for
  write/append (where implemented, currently for file and clipboard
  connections, \code{\link{stdout}} and \code{\link{stderr}}).

  If for a \code{file} or \code{fifo} connection the description is
  \code{""}, the file/fifo is immediately opened (in \code{"w+"} mode
  unless \code{open = "w+b"} is specified) and unlinked from the file
  system.  This provides a temporary file/fifo to write to and then read
  from.
}

\section{URLs}{
  \code{url} and \code{file} support URL schemes \samp{http://},
  \samp{ftp://} and \samp{file://}.

  A note on \samp{file://} URLs.  The most general form (from RFC1738) is
  \samp{file://host/path/to/file}, but \R only accepts the form with an
  empty \code{host} field referring to the local machine.

  On a Unix-alike, this is then \samp{file:///path/to/file}, where
  \samp{path/to/file} is relative to \file{/}.  So although the third
  slash is strictly part of the specification not part of the path, this
  can be regarded as a way to specify the file \file{/path/to/file}.  It
  is not possible to specify a relative path using a file URL.

  In this form the path is relative to the root of the filesystem, not a
  Windows concept.  The standard form on Windows is
  \samp{file:///d:/R/repos}: for compatibility with earlier versions of
  \R and Unix versions, any other form is parsed as \R as \samp{file://}
  plus \code{path_to_file}.  Also, backslashes are accepted within the
  path even though RFC1738 does not allow them.

  No attempt is made to decode an encoded URL: call
  \code{\link{URLdecode}} if necessary.

  Note that \samp{https://} connections are not supported except on
  Windows.  There they are only supported if \option{--internet2} or
#ifdef unix
  \code{setInternet2(TRUE)}
#endif
#ifdef windows
  \code{\link{setInternet2}(TRUE)}
#endif
  was used (to make use of Internet Explorer internals), and then only
  if the certificate is considered to be valid.  With that option only,
  the \samp{http://user:pass@site} notation for sites requiring
  authentication is also accepted.

  Contributed package \CRANpkg{RCurl} provides more comprehensive facilities
  to download from URLs.
}

\value{
  \code{file}, \code{pipe}, \code{fifo}, \code{url}, \code{gzfile},
  \code{bzfile}, \code{xzfile}, \code{unz} and \code{socketConnection}
  return a connection object which inherits from class
  \code{"connection"} and has a first more specific class.
  
  \code{open} and \code{flush} return \code{NULL}, invisibly.

  \code{close} returns either \code{NULL} or an integer status,
  invisibly. The status is from when the connection was last closed and
  is available only for some types of connections (e.g. pipes, files and
  fifos): typically zero values indicate success.
  
  \code{isOpen} returns a logical value, whether the connection is
  currently open.

  \code{isIncomplete} returns a logical value, whether the last read
  attempt was blocked, or for an output text connection whether there is
  unflushed output.
}

\section{Modes}{
  Possible values for the argument \code{open} are
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
  opened for both reading and writing.  An unsupported mode is usually
  silently substituted.

  If a file or fifo is created on a Unix-alike, its permissions will be
  the maximal allowed by the current setting of \code{umask} (see
  \code{\link{Sys.umask}}).

  For many connections there is little or no difference between text and
  binary modes.  For file-like connections on Windows, translation of
  line endings (between LF and CRLF) is done in text mode only (but text
  read operations on connections such as \code{\link{readLines}},
  \code{\link{scan}} and \code{\link{source}} work for any form of line
  ending).  Various \R operations are possible in only one of the modes:
  for example \code{\link{pushBack}} is text-oriented and is only
  allowed on connections open for reading in text mode, and binary
  operations such as \code{\link{readBin}}, \code{\link{load}} and
  \code{\link{save}} operations can only be done on binary-mode
  connections.

  The mode of a connection is determined when actually opened, which is
  deferred if \code{open = ""} is given (the default for all but socket
  connections).  An explicit call to \code{open} can specify the mode,
  but otherwise the mode will be \code{"r"}.  (\code{gzfile},
  \code{bzfile} and \code{xzfile} connections are exceptions, as the
  compressed file always has to be opened in binary mode and no
  conversion of line-endings is done even on Windows, so the default
  mode is interpreted as \code{"rb"}.)  Most operations that need write
  access or text-only or binary-only mode will override the default mode
  of a non-yet-open connection.

  Append modes need to be considered carefully for compressed-file
  connections.  They do \strong{not} produce a single compressed stream
  on the file, but rather append a new compressed stream to the file.
  Readers (including \R) may or may not read beyond end of the first
  stream: currently \R does so for \code{gzfile}, \code{bzfile} and
  \code{xzfile} connections, but earlier versions did not.
}

\section{Compression}{
  \R has for a long time supported \command{gzip} and \command{bzip2}
  compression, and support for \command{xz} compression (and read-only
  support for its precursor \code{lzma} compression) was added in \R
  2.10.0.

  For reading, the type of compression (if any) can be determined from
  the first few bytes of the file, and this is exploited as from \R
  2.10.0.  Thus for \code{file(raw = FALSE)} connections, if \code{open} is
  \code{""}, \code{"r"} or \code{"rt"} the connection can read any of
  the compressed file types as well as uncompressed files.  (Using
  \code{"rb"} will allow compressed files to be read byte-by-byte.)
  Similarly, \code{gzfile} connections can read any of the forms of
  compression and uncompressed files in any read mode.

  (The type of compression is determined when the connection is created
  if \code{open} is unspecified and a file of that name exists.  If the
  intention is to open the connection to write a file with a
  \emph{different} form of compression under that name, specify
  \code{open = "w"} when the connection is created or
  \code{\link{unlink}} the file before creating the connection.)

  For write-mode connections, \code{compress} specifies how hard the
  compressor works to minimize the file size, and higher values need
  more CPU time and more working memory (up to ca 800Mb for
  \code{xzfile(compress = 9)}).  For \code{xzfile} negative values of
  \code{compress} correspond to adding the \command{xz} argument
  \option{-e}: this takes more time (double?) to compress but may
  achieve (slightly) better compression.  The default (\code{6}) has
  good compression and modest (100Mb memory usage): but if you are using
  \code{xz} compression you are probably looking for high compression.

  Choosing the type of compression involves tradeoffs: \command{gzip},
  \command{bzip2} and \command{xz} are successively less widely supported,
  need more resources for both compression and decompression, and
  achieve more compression (although individual files may buck the
  general trend).  Typical experience is that \code{bzip2} compression
  is 15\% better on text files than \code{gzip} compression, and
  \code{xz} with maximal compression 30\% better.  The experience with
  \R \code{\link{save}} files is similar, but on some large \file{.rda}
  files \code{xz} compression is much better than the other two.  With
  current computers decompression times even with \code{compress = 9}
  are typically modest and reading compressed files is usually faster
  than uncompressed ones because of the reduction in disc activity.
}

\section{Encoding}{
  The encoding of the input/output stream of a connection can be
  specified by name in the same way as it would be given to
  \code{\link{iconv}}: see that help page for how to find out what
  encoding names are recognized on your platform.  Additionally,
  \code{""} and \code{"native.enc"} both mean the \sQuote{native}
  encoding, that is the internal encoding of the current locale and
  hence no translation is done.

  Re-encoding only works for connections in text mode: reading from a
  connection with re-encoding specified in binary mode will read the
  stream of bytes, but mixing text and binary mode reads (e.g. mixing
  calls to \code{\link{readLines}} and \code{\link{readChar}}) is likely
  to lead to incorrect results.

  The encodings \code{"UCS-2LE"} and \code{"UTF-16LE"} are treated
  specially, as they are appropriate values for Windows \sQuote{Unicode}
  text files.  If the first two bytes are the Byte Order Mark
  \code{0xFFFE} then these are removed as some implementations of
  \code{\link{iconv}} do not accept BOMs.  Note that whereas most
  implementations will handle BOMs using encoding \code{"UCS-2"} and
  choose the appropriate byte order, some (including earlier versions of
  \code{glibc}) will not.  There is a subtle distinction between
  \code{"UTF-16"} and \code{"UCS-2"} (see
  \url{http://en.wikipedia.org/wiki/UTF-16/UCS-2}: the use of surrogate
  pairs is very rare so \code{"UCS-2LE"} is an appropriate first choice.

  As from \R 3.0.0 the encoding \code{"UTF-8-BOM"} is accepted for
  reading and will remove a Byte Order Mark if present (which it often
  is for files and webpages generated by Microsoft applications).  If it
  is required when writing it should be written explicitly, e.g. by
  \code{writeChar("\ufeff", con, eos = NULL)} or
  \code{writeBin(as.raw(c(0xef, 0xbb, 0xff)), con)}

  Requesting a conversion that is not supported is an error, reported
  when the connection is opened.  Exactly what happens when the
  requested translation cannot be done for invalid input is in general
  undocumented.  On output the result is likely to be that up to the
  error, with a warning.  On input, it will most likely be all or some
  of the input up to the error.

  It may be possible to deduce the current native encoding from
  \code{\link{Sys.getlocale}("LC_CTYPE")}, but not all OSes record it.
}

\section{Blocking}{
  Whether or not the connection blocks can be specified for file, url
  (default yes) fifo and socket connections (default not).

  In blocking mode, functions using the connection do not return to the
  \R evaluator until the read/write is complete.  In non-blocking mode,
  operations return as soon as possible, so on input they will return
  with whatever input is available (possibly none) and for output they
  will return whether or not the write succeeded.

  The function \code{\link{readLines}} behaves differently in respect of
  incomplete last lines in the two modes: see its help page.

  Even when a connection is in blocking mode, attempts are made to
  ensure that it does not block the event loop and hence the operation
  of GUI parts of \R.  These do not always succeed, and the whole \R
  process will be blocked during a DNS lookup on Unix, for example.

  Most blocking operations on HTTP/FTP URLs and on sockets are subject to the
  timeout set by \code{options("timeout")}.  Note that this is a timeout
  for no response, not for the whole operation.  The timeout is set at
  the time the connection is opened (more precisely, when the last
  connection of that type -- \samp{http:}, \samp{ftp:} or socket -- was
  opened).
}

\section{Fifos}{
  Fifos default to non-blocking.  That follows S version 4 and is
  probably most natural, but it does have some implications.  In
  particular, opening a non-blocking fifo connection for writing (only)
  will fail unless some other process is reading on the fifo.

  Opening a fifo for both reading and writing (in any mode: one can only
  append to fifos) connects both sides of the fifo to the \R process,
  and provides an similar facility to \code{file()}.
}

\section{Clipboard}{
  \code{file} can be used with \code{description = "clipboard"}
#ifdef windows
  in modes \code{"r"} and \code{"w"} only.
#endif
#ifdef unix
  in mode \code{"r"} only.  This reads the X11 primary selection (see
  \url{http://standards.freedesktop.org/clipboards-spec/clipboards-latest.txt}),
  which can also be specified as \code{"X11_primary"} and the secondary
  selection as \code{"X11_secondary"}.  On most systems the clipboard
  selection (that used by \sQuote{Copy} from an \sQuote{Edit} menu) can
  be specified as \code{"X11_clipboard"}.
#endif

  When a clipboard is opened for reading, the contents are immediately
  copied to internal storage in the connection.

#ifdef windows
  When writing to the clipboard, the output is copied to the clipboard
  only when the connection is closed or flushed.  There is a 32Kb limit
  on the text to be written to the clipboard.  This can be raised by
  using e.g. \code{file("clipboard-128")} to give 128Kb.

  The clipboard works in Unicode wide characters, so encodings might
  not work as one might expect.
#endif
#ifdef unix
  Unix users wishing to \emph{write} to one of the selections may be
  able to do so via \code{xclip}
  (\url{http://sourceforge.net/projects/xclip/}), for example by
  \code{pipe("xclip -i", "w")} for the primary selection.

  OS X users can use \code{pipe("pbpaste")} and
  \code{pipe("pbcopy", "w")} to read from and write to that system's
  clipboard.
#endif
}
\note{
  \R's connections are modelled on those in S version 4 (see Chambers,
  1998).  However \R goes well beyond the S model, for example in output
  text connections and URL, compressed and socket connections.

  The default open mode in \R is \code{"r"} except for socket connections.
  This differs from S, where it is the equivalent of \code{"r+"},
  known as \code{"*"}.

  On (rare) platforms where \code{vsnprintf} does not return the needed length
  of output there is a 100,000 byte output limit on the length of
  line for text output on \code{fifo}, \code{gzfile}, \code{bzfile} and
  \code{xzfile} connections: longer lines will be truncated with a
  warning.
}
\references{
  Chambers, J. M. (1998)
  \emph{Programming with Data.  A Guide to the S Language.} Springer.

  Ripley, B. D. (2001) Connections. \emph{R News}, \bold{1/1}, 16--7.
  \url{http://www.r-project.org/doc/Rnews/Rnews_2001-1.pdf}
}

\seealso{
  \code{\link{textConnection}}, \code{\link{seek}},
  \code{\link{showConnections}}, \code{\link{pushBack}}.

  Functions making direct use of connections are (text-mode)
  \code{\link{readLines}}, \code{\link{writeLines}}, \code{\link{cat}},
  \code{\link{sink}}, \code{\link{scan}}, \code{\link{parse}},
  \code{\link{read.dcf}}, \code{\link{dput}}, \code{\link{dump}} and
  (binary-mode) \code{\link{readBin}}, \code{\link{readChar}},
  \code{\link{writeBin}}, \code{\link{writeChar}}, \code{\link{load}}
  and \code{\link{save}}.

  \code{\link{capabilities}} to see if HTTP/FTP \code{url},
  \code{fifo} and \code{socketConnection} are supported by this build of \R.

  \code{\link{gzcon}} to wrap \command{gzip} (de)compression around a
  connection.

  \code{\link{memCompress}} for more ways to (de)compress and references
  on data compression.
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
readLines(zz <- gzfile("ex.gz"))
close(zz)
unlink("ex.gz")

zz <- bzfile("ex.bz2", "w")  # bzip2-ed file
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file = zz, sep = "\n")
close(zz)
print(readLines(zz <- bzfile("ex.bz2")))
close(zz)
unlink("ex.bz2")

## An example of a file open for reading and writing
Tfile <- file("test1", "w+")
c(isOpen(Tfile, "r"), isOpen(Tfile, "w")) # both TRUE
cat("abc\ndef\n", file = Tfile)
readLines(Tfile)
seek(Tfile, 0, rw = "r") # reset to beginning
readLines(Tfile)
cat("ghi\n", file = Tfile)
readLines(Tfile)
close(Tfile)
unlink("test1")

## We can do the same thing with an anonymous file.
Tfile <- file()
cat("abc\ndef\n", file = Tfile)
readLines(Tfile)
close(Tfile)

\donttest{## fifo example -- may fail even with OS support for fifos
if(capabilities("fifo")) {
  zz <- fifo("foo-fifo", "w+")
  writeLines("abc", zz)
  print(readLines(zz))
  close(zz)
  unlink("foo-fifo")
}}
#ifdef unix
\donttest{
## Unix examples of use of pipes

# read listing of current directory
readLines(pipe("ls -1"))

# remove trailing commas.  Suppose
\dontshow{
writeLines(c("450, 390, 467, 654,  30, 542, 334, 432, 421,",
"357, 497, 493, 550, 549, 467, 575, 578, 342,",
"446, 547, 534, 495, 979, 479"), "data2_")}
\dontrun{\% cat data2_
450, 390, 467, 654,  30, 542, 334, 432, 421,
357, 497, 493, 550, 549, 467, 575, 578, 342,
446, 547, 534, 495, 979, 479}
# Then read this by
scan(pipe("sed -e s/,$// data2_"), sep = ",")
\dontshow{unlink("data2_")}

# convert decimal point to comma in output: see also write.table
# both R strings and (probably) the shell need \ doubled
zz <- pipe(paste("sed s/\\\\\\\\./,/ >", "outfile"), "w")
cat(format(round(stats::rnorm(48), 4)), fill = 70, file = zz)
close(zz)
file.show("outfile", delete.file = TRUE)

## example for a machine running a finger daemon

con <- socketConnection(port = 79, blocking = TRUE)
writeLines(paste0(system("whoami", intern = TRUE), "\r"), con)
gsub(" *$", "", readLines(con))
close(con)
}
#endif

\dontrun{
## Two R processes communicating via non-blocking sockets
# R process 1
con1 <- socketConnection(port = 6011, server = TRUE)
writeLines(LETTERS, con1)
close(con1)

# R process 2
con2 <- socketConnection(Sys.info()["nodename"], port = 6011)
# as non-blocking, may need to loop for input
readLines(con2)
while(isIncomplete(con2)) {
   Sys.sleep(1)
   z <- readLines(con2)
   if(length(z)) print(z)
}
close(con2)

## examples of use of encodings
# write a file in UTF-8
cat(x, file = (con <- file("foo", "w", encoding = "UTF-8"))); close(con)
# read a 'Windows Unicode' file
A <- read.table(con <- file("students", encoding = "UCS-2LE")); close(con)
}}
\keyword{file}
\keyword{connection}
