\name{serialize}
\alias{.readRDS}
\alias{.saveRDS}
\alias{serialize}
\alias{unserialize}
\title{Simple Serialization Interface}
\description{
  A simple low level interface for serializing to connections.
}
\usage{
serialize(object, connection, ascii = FALSE, refhook = NULL)
unserialize(connection, refhook = NULL)
.saveRDS(object, file = "", ascii = FALSE, version = NULL,
         compress = FALSE, refhook = NULL)
.readRDS(file, refhook = NULL)
}
\arguments{
  \item{object}{\R object to serialize.}
  \item{file}{a connection or the name of the file where the R object
    is saved to or read from, or \code{NULL} for \code{serialize}
    (see details).}
  \item{ascii}{a logical.  If \code{TRUE}, an ASCII representation is
    written; otherwise (default), a more compact binary one is used.}
  \item{version}{the workspace format version to use.  \code{NULL}
    specifies the current default format.  The version used from \R
    0.99.0 to \R 1.3.1 was version 1.  The default format as from \R
    1.4.0 is version 2.}
  \item{compress}{a logical specifying whether saving to a named file is
    to use compression.  Ignored when \code{file} is a connection and for
    workspace format version 1.}
  \item{connection}{an open connection.}
  \item{refhook}{a hook function for handling reference objects.}
}
\details{
  The function \code{serialize} writes \code{object} to the specified
  connection.  Sharing of reference objects is preserved within the
  object but not across separate calls to serialize.  If
  \code{connection} is \code{NULL} then \code{object} is serialized to a
  scalar string, which is returned as the result of \code{serialize}.
  For a text mode connection, the default value of \code{ascii} is set
  to \code{TRUE}.

  \code{unserialize} reads an object from \code{connection}.
  \code{connection} may also be a scalar string.

  The \code{refhook} functions can be used to customize handling of
  non-system reference objects (all external pointers and weak
  references, and all environments other than name space and package
  environments and \code{.GlobalEnv}).  The hook function for
  \code{serialize} should return a character vector for references it
  wants to handle; otherwise it should return \code{NULL}.  The hook for
  \code{unserialize} will be called with character vectors supplied to
  \code{serialize} and should return an appropriate object.
}
\section{Warning}{
  These functions are still experimental.  Both names, interfaces and
  values might change in future versions.  \code{.saveRDS} and
  \code{.readRDS} are intended for internal use.
}
\value{
  For \code{serialize}, \code{NULL} unless \code{connection=NULL}, when
  the result is stored in the first element of a character vector (but
  is not a normal character string unless \code{ascii = TRUE} and should
  not be processed except by \code{unserialize}).

  For \code{unserialize} and \code{.readRDS}, an \R object.

  For \code{.saveRDS}, \code{NULL} invisibly.
}
\examples{
x <- serialize(list(1,2,3), NULL)
unserialize(x)
}
\keyword{internal}
\keyword{file}
