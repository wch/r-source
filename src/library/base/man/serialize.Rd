\name{serialize}
\alias{.readRDS}
\alias{.saveRDS}
\title{Simple Serialization Interface}
\description{
  A simple interface for serializing to connections.
}
\usage{
.saveRDS(object, file = "", ascii = FALSE, version = NULL,
         compress = FALSE, refhook = NULL) 
.readRDS(file, refhook = NULL) 
}
\arguments{
  \item{object}{the name of the \R object to serialize.}
  \item{file}{a connection or the name of the file where the R object
    is saved to or read from.}
  \item{ascii}{a logical.  If \code{TRUE}, an ASCII representation is
    written; otherwise (default), a more compact binary one is used.} 
  \item{version}{the workspace format version to use.  \code{NULL}
    specifies the current default format.  The version used from \R
    0.99.0 to \R 1.3.1 was version 1.  The default format as from \R
    1.4.0 is version 2.}
  \item{compress}{a logical specifying whether saving to a named file is
    to use compression.  Ignored when \code{file} is a connection and for
    workspace format version 1.}
  \item{refhook}{a hook function for handling reference objects.}
}
\section{Warning}{
  These functions are still experimental.  Both names, interfaces and
  values might change in future versions.
}
\keyword{internal}
\keyword{file}
