% File src/library/base/man/serialize.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2011 R Core Development Team
% Distributed under GPL 2 or later

% These were introduced in R 1.7.0, post version 2
\name{readRDS}
\alias{.readRDS}
\alias{.saveRDS}
\title{Internal Serialization Interface}
\description{
  Internal functions for serialization of \R objects to a file.  See
  \code{\link{serialize}} for public versions.
}
\usage{
.saveRDS(object, file = "", ascii = FALSE, version = NULL,
         compress = TRUE, refhook = NULL)

.readRDS(file, refhook = NULL)
}
\arguments{
  \item{object}{\R object to serialize.}
  \item{file}{a connection or the name of the file where the R object
    is saved to or read from.}
  \item{ascii}{a logical.  If \code{TRUE}, an ASCII representation is
    written; otherwise (default except for text-mode connections), a
    more compact binary one is used.}
  \item{version}{the workspace format version to use.  \code{NULL}
    specifies the current default version (2).  Earlier versions are not
    supported, so this will only be relevant when there are later versions.}
  \item{compress}{a logical specifying whether saving to a named file is
    to use \code{"gzip"} compression.  As from \R 2.10.0
    this is also allowed to be \code{"bzip2"} to specify that type of
    compression, and from \R 2.13.0, \code{"xz"}.}
  \item{refhook}{a hook function for handling reference objects.}
}
\details{
  Since these are internal, the file format is subject to change without
  notice.  The current file format is that of \code{\link{serialize}},
  optionally compressed.
}

\value{
  For \code{.readRDS}, an \R object.

  For \code{.saveRDS}, \code{NULL} invisibly.
}
\keyword{internal}
