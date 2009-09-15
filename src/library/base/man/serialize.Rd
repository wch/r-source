% File src/library/base/man/serialize.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{serialize}
\alias{serialize}
\alias{unserialize}
\title{Simple Serialization Interface}
\description{
  A simple low-level interface for serializing to connections.
}
\usage{
serialize(object, connection, ascii = FALSE, refhook = NULL)

unserialize(connection, refhook = NULL)
}
\arguments{
  \item{object}{\R object to serialize.}
  \item{connection}{an open connection or (for \code{serialize})
    \code{NULL} or (for \code{unserialize}) a raw vector
    (see \sQuote{Details}).}
  \item{ascii}{a logical.  If \code{TRUE}, an ASCII representation is
    written; otherwise (default except for text-mode connections), a
    more compact binary one is used.}
  \item{refhook}{a hook function for handling reference objects.}
}
\details{
  The function \code{serialize} writes \code{object} to the specified
  connection.  If \code{connection} is \code{NULL} then \code{object} is
  serialized to a raw vector, which is returned as the result of
  \code{serialize}.
  
  Sharing of reference objects is preserved within the object but not
  across separate calls to \code{serialize}.

  \code{unserialize} reads an object (as written by \code{serialize})
  from \code{connection} or a raw vector.

  The \code{refhook} functions can be used to customize handling of
  non-system reference objects (all external pointers and weak
  references, and all environments other than name space and package
  environments and \code{.GlobalEnv}).  The hook function for
  \code{serialize} should return a raw vector for references it
  wants to handle; otherwise it should return \code{NULL}.  The hook for
  \code{unserialize} will be called with raw vectors supplied to
  \code{serialize} and should return an appropriate object.

  For a text-mode connection, the default value of \code{ascii} is set
  to \code{TRUE}: only ASCII representations can be written to text-mode
  connections and attempting to use \code{ascii = FALSE} will throw an
  error.
  
  The format consists of a single line followed by the data: the first
  line contains a single character: \code{X} for binary serialization
  and \code{A} for ASCII serialization, followed by a new line.
}
\section{Warning}{
  These functions are still experimental.  Names, interfaces and
  values might change in future versions (and the value of
  \code{serialize} was changed for \R 2.4.0).

  A raw vector is limited to \eqn{2^{31} - 1}{2^31 - 1} bytes, but \R
  objects can exceed this and their serializations will normally be
  larger than the objects.
}
\value{
  For \code{serialize}, \code{NULL} unless \code{connection = NULL}, when
  the result is returned in a raw vector.

  For \code{unserialize} an \R object.
}
\examples{
x <- serialize(list(1,2,3), NULL)
unserialize(x)
}
\keyword{file}
\keyword{connection}
