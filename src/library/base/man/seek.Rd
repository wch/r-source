\name{seek}
\alias{seek}
\alias{seek.connection}
\alias{truncate}
\alias{truncate.connection}
\alias{isSeekable}
\title{Functions to Reposition Connections}
\description{
  Functions to re-position connections.
}
\usage{
seek(con, \dots)
\method{seek}{connection}(con, where = NA, origin = "start", rw = "", \dots)

isSeekable(con)

truncate(con, \dots)
}
\arguments{
  \item{con}{a connection.}
  \item{where}{integer.  A file position (relative to the origin
    specified by \code{origin}), or \code{NA}.}
  \item{rw}{character.  Empty or \code{"read"} or \code{"write"},
    partial matches allowed.}
  \item{origin}{character.  One of \code{"start"}, \code{"current"},
    \code{"end"}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{seek} with \code{where = NA} returns the current byte offset
  of a connection (from the beginning), and with a non-missing \code{where}
  argument the connection is re-positioned (if possible) to the
  specified position. \code{isSeekable} returns whether the connection
  in principle supports \code{seek}: currently only (possibly
  gz-compressed) file connections do.  \code{gzfile} connections do not
  support \code{origin = "end"}, and the file position they use is that
  of the uncompressed file.
  
#ifdef windows
  The value returned by \code{seek(where=NA)} appears to be unreliable
  on Windows systems, at least for text files.  Clipboard connections
  can seek too.
#endif
  
  File connections can be open for both writing/appending, in which case
  \R keeps separate positions for reading and writing.  Which \code{seek}
  refers to can be set by its \code{rw} argument: the default is the
  last mode (reading or writing) which was used.  Most files are
  only opened for reading or writing and so default to that state.
  If a file is open for reading and writing but has not been used, the
  default is to give the reading position (0).

  The initial file position for reading is always at the beginning.
  The initial position for writing is at the beginning of the file
  for modes \code{"r+"} and \code{"r+b"}, otherwise at the end of the
  file.  Some platforms only allow writing at the end of the file in
  the append modes.

  \code{truncate} truncates a file opened for writing at its current
  position.  It works only for \code{file} connections, and is not
  implemented on all platforms.
}
\value{
  \code{seek} returns the current position (before any move),
  as a byte offset, if relevant, or \code{0} if not.

  \code{truncate} returns \code{NULL}: it stops with an error if
  it fails (or is not implemented).

  \code{isSeekable} returns a logical value, whether the connection is
  support \code{seek}.
}
\seealso{
  \code{\link{connections}}
}
\keyword{file}
\keyword{connection}
