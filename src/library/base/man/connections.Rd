\name{connection}
\alias{file}
\alias{pipe}
\alias{textConnection}
\alias{open}
\alias{open.connection}
\alias{isOpen}
\alias{isIncomplete}
\alias{close}
\alias{close.connection}
\alias{seek}
\alias{seek.connection}
\alias{print.connection}
\alias{summary.connection}
\title{Functions to Manipulate Connections}
\description{
  Functions to create, open, close and position connections.
}
\usage{
file(description, open = "", blocking = TRUE)
pipe(description, open = "")
textConnection(description, open =  "")

open(con, open = "rt", blocking = TRUE)
close(con, type = "rw")
seek(con, where = NA, rw = "")
isOpen(con, rw = "")
isIncomplete(con)
}
\arguments{
  \item{description}{character. A description of the connection. For
    \code{file} this is a path to the file to be opened.
    For a \code{textConnection} it is an \R character vector object.
  }
  \item{open}{character.  A description of how to open the connection
    (if at all). See Details for possible values.}
  \item{blocking}{logical. Currently ignored.}
  \item{type}{character. Currently ignored.}
  \item{where}{integer.  A file position, or \code{NA}.}
  \item{rw}{character.  Currently ignored.}
}
\details{
  The first three functions create connections.  By default the connection
  is not opened, but may be opened by setting a non-empty value of
  argument \code{open}.

  \code{open}, \code{close} and \code{seek} are generic functions: the
  following applies to the methods relevant to connections.
  
  \code{open} opens a connection.  In general functions using
  connections will open them if they are not open, but then close them
  again, so to leave a connection open call \code{open} explicitly.

  \code{close} closes and destroys a connection.

  \code{seek} with \code{where = NA} returns the current byte offset
  of a connection (from the beginning), and with a positive \code{where}
  argument the connection is re-positioned (if possible) to the
  specified position.

  Text connections \emph{are} opened and the character vector is copied at the
  time the connection object is created, and \code{close} destroys the copy.
  You cannot \code{seek} on a text connection, and \code{seek} will
  always return zero as the position.

  Possible values for the mode \code{open} to open a connection are
  \describe{
    \item{\code{"r"}}{or \code{"rt"}. Open for reading in text mode.}
    \item{\code{"w"}}{or \code{"wt"}. Open for writing in text mode.}
    \item{\code{"a"}}{or \code{"at"}. Open for appending in text mode.}
    \item{\code{"rb"}}{Open for reading in binary mode.}
    \item{\code{"wb"}}{Open for writing in binary mode.}
    \item{\code{"ab"}}{Open for appending in binary mode.}
  }
  \emph{These are likely to change.}  Some connections (e.g. text
  connections) can only be opened for reading or writing.  For many
  connections there is little or no difference between text and binary
  modes, but there is for file-like connections on Windows, and
  \code{\link{pushBack}} is text-oriented and is only allowed on
  connections open for reading in text mode.
}
\value{
  \code{file}, \code{pipe} and  \code{textConnection} 
  return a connection object which inherits from class
  \code{"connection"}.

  \code{seek} returns the current position, as a byte offset, if
  relevant.

  \code{isOpen} returns a logical value, whether the connection is
  currently open.

  \code{isIncomplete} returns a logical value, whether last read attempt
  was blocked (currently always false).
}
\note{
  \R's connections are modelled on those in S version 4 (see Chamber,
  1998), but the implementation is currently incomplete.  In particular:
  \itemize{
    \item Pipes are only implemented on Unix. Fifos are not yet
    implemented. We also envisage having connections to sockets.

    \item Svr4 has separate read and write positions for files: \R does
    not.

    \item Svr4 allow an empty description to \code{file} to indicate an
    anonymous (temporary) file.
  }
}
\references{
  Chambers, J. M. (1998)
  \emph{Programming with Data. A Guide to the S Language.} Springer.
}

\seealso{
  \code{\link{readLines}}, \code{\link{showConnections}},
  \code{\link{pushBack}}
}

\examples{
zz <- textConnection(LETTERS)
readLines(zz, 2)
scan(zz, "", 4)
pushBack(c("aa", "bb"), zz)
scan(zz, "", 4)
close(zz)
}
\keyword{file}
\keyword{connection}
