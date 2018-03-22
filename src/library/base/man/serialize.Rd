% File src/library/base/man/serialize.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{serialize}
\alias{serialize}
\alias{unserialize}
\title{Simple Serialization Interface}
\description{
  A simple low-level interface for serializing to connections.
}
\usage{
serialize(object, connection, ascii, xdr = TRUE,
          version = NULL, refhook = NULL)

unserialize(connection, refhook = NULL)
}
\arguments{
  \item{object}{\R object to serialize.}
  \item{connection}{an open \link{connection} or (for \code{serialize})
    \code{NULL} or (for \code{unserialize}) a raw vector
    (see \sQuote{Details}).}
  \item{ascii}{a logical.  If \code{TRUE} or \code{NA}, an ASCII
    representation is written; otherwise (default) a binary one.
    See also the comments in the help for \code{\link{save}}.}
  \item{xdr}{a logical: if a binary representation is used, should a
    big-endian one (XDR) be used?}
  \item{version}{the workspace format version to use.  \code{NULL}
    specifies the current default version (2), which has been the default
    since \R 1.4.0. The only other supported value is 3, introduced in
    \R 3.5.0.}
  \item{refhook}{a hook function for handling reference objects.}
}
\details{
  The function \code{serialize} serializes \code{object} to the specified
  connection.  If \code{connection} is \code{NULL} then \code{object} is
  serialized to a raw vector, which is returned as the result of
  \code{serialize}.

  Sharing of reference objects is preserved within the object but not
  across separate calls to \code{serialize}.

  \code{unserialize} reads an object (as written by \code{serialize})
  from \code{connection} or a raw vector.

  The \code{refhook} functions can be used to customize handling of
  non-system reference objects (all external pointers and weak
  references, and all environments other than namespace and package
  environments and \code{.GlobalEnv}).  The hook function for
  \code{serialize} should return a character vector for references it
  wants to handle; otherwise it should return \code{NULL}.  The hook for
  \code{unserialize} will be called with character vectors supplied to
  \code{serialize} and should return an appropriate object.

  For a text-mode connection, the default value of \code{ascii} is set
  to \code{TRUE}: only ASCII representations can be written to text-mode
  connections and attempting to use \code{ascii = FALSE} will throw an
  error.

  The format consists of a single line followed by the data: the first
  line contains a single character: \code{X} for binary serialization
  and \code{A} for ASCII serialization, followed by a new line.  (The
  format used is identical to that used by \code{\link{readRDS}}.)

  As almost all systems in current use are little-endian, \code{xdr =
  FALSE} can be used to avoid byte-shuffling at both ends when
  transferring data from one little-endian machine to another (or
  between processes on the same machine).  Depending on the system, this
  can speed up serialization and unserialization by a factor of up to
  3x.
}
\section{Warning}{
  These functions have provided a stable interface since \R 2.4.0 (when
  the storage of serialized objects was changed from character to raw
  vectors).  However, the serialization format may change in future
  versions of \R, so this interface should not be used for long-term
  storage of \R objects.

  On 32-bit platforms a raw vector is limited to \eqn{2^{31} - 1}{2^31 -
  1} bytes, but \R objects can exceed this and their serializations will
  normally be larger than the objects.
}
\value{
  For \code{serialize}, \code{NULL} unless \code{connection = NULL}, when
  the result is returned in a raw vector.

  For \code{unserialize} an \R object.
}
\seealso{
  \code{\link{saveRDS}} for a more convenient interface to serialize an
  object to a file or connection.

  \code{\link{save}} and \code{\link{load}} to serialize and restore one
  or more named objects.

  The \sQuote{R Internals} manual for details of the format used.
}
\examples{
x <- serialize(list(1,2,3), NULL)
unserialize(x)

## see also the examples for saveRDS
}
\keyword{file}
\keyword{connection}
