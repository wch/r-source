% File src/library/base/man/serialize.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Team
% Distributed under GPL 2 or later

\name{readRDS}
\alias{readRDS}
\alias{saveRDS}
\title{Serialization Interface for Single Objects}
\description{
  Functions to write a single \R object to a file, and to restore it.
}
\usage{
saveRDS(object, file = "", ascii = FALSE, version = NULL,
        compress = TRUE, refhook = NULL)

readRDS(file, refhook = NULL)
}
\arguments{
  \item{object}{\R object to serialize.}
  \item{file}{a \link{connection} or the name of the file where the R object
    is saved to or read from.}
  \item{ascii}{a logical.  If \code{TRUE}, an ASCII representation is
    written; otherwise (default except for text-mode connections), a
    binary one is used.  See the comments in the help for \code{\link{save}}.}
  \item{version}{the workspace format version to use.  \code{NULL}
    specifies the current default version (2).  Versions prior to 2 are not
    supported, so this will only be relevant when there are later versions.}
  \item{compress}{a logical specifying whether saving to a named file is
    to use \code{"gzip"} compression, or one of \code{"gzip"},
    \code{"bzip2"} or \code{"xz"} to indicate the type of compression to
    be used.  Ignored if \code{file} is a connection.}
  \item{refhook}{a hook function for handling reference objects.}
}
\details{
  These functions provide the means to save a single \R object to a
  connection (typically a file) and to restore the object, quite
  possibly under a different name.  This differs from \code{\link{save}}
  and \code{\link{load}}, which save and restore one or more named
  objects into an environment.  They are widely used by \R itself, for
  example to store metadata for a package and to store the
  \code{\link{help.search}} databases: the \code{".rds"} file extension
  is most often used.

  Functions \code{\link{serialize}} and \code{\link{unserialize}}
  provide a slightly lower-level interface to serialization: objects
  serialized to a connection by \code{serialize} can be read back by
  \code{readRDS} and conversely.

  All of these interfaces use the same serialization format, which has
  been used since \R 1.4.0 (but extended from time to time as new
  object types have been added to \R).  However, \code{save} writes a
  single line header (typically \code{"RDXs\n"}) before the
  serialization of a single object (a pairlist of all the objects to be
  saved).

  Compression is handled by the connection opened when \code{file} is a
  file name, so is only possible when \code{file} is a connection if
  handled by the connection.  So e.g. \code{\link{url}} connections will
  needed to be wrapped in a call to \code{\link{gzcon}}.

  If a connection is supplied it will be opened (in binary mode) for the
  duration of the function if not already open: if it is already open it
  must be in binary mode for \code{saveRDS(ascii = FALSE)} (the
  default).
}

\value{
  For \code{readRDS}, an \R object.

  For \code{saveRDS}, \code{NULL} invisibly.
}

\seealso{
  \code{\link{serialize}}, \code{\link{save}} and \code{\link{load}}.

  The \sQuote{R Internals} manual for details of the format used.
}

\examples{
## save a single object to file
saveRDS(women, "women.rds")
## restore it under a different name
women2 <- readRDS("women.rds")
identical(women, women2)
## or examine the object via a connection, which will be opened as needed.
con <- gzfile("women.rds")
str(readRDS(con))
close(con)

## Less convenient ways to restore the object
## which demonstrate compatibility with unserialize()
con <- gzfile("women.rds", "rb")
identical(unserialize(con), women)
close(con)
con <- gzfile("women.rds", "rb")
wm <- readBin(con, "raw", n = 1e4) # size is a guess
close(con)
identical(unserialize(wm), women)

## Format compatibility with serialize():
con <- file("women2", "w")
serialize(women, con) # ASCII, uncompressed
close(con)
identical(women, readRDS("women2"))
con <- bzfile("women3", "w")
serialize(women, con) # binary, bzip2-compressed
close(con)
identical(women, readRDS("women2"))

\testonly{unlink(c("women.rds", "women2", "women3"))}
}

\keyword{file}
\keyword{connection}
