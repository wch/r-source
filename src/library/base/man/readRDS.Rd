% File src/library/base/man/serialize.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2009 R Core Development Team
% Distributed under GPL 2 or later

\name{readRDS}
\alias{.readRDS}
\alias{.saveRDS}
\title{Internal Serialization Interface}
\description{
  Internal functions for serialization.  See \code{\link{serialize}} for
  public (but experimental) versions.
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
    specifies the current default format.  The version used from \R
    0.99.0 to \R 1.3.1 was version 1.  The default format as from \R
    1.4.0 is version 2.}
  \item{compress}{a logical specifying whether saving to a named file is
    to use compression.  Ignored when \code{file} is a connection and for
    workspace format version 1.  As from \R 2.10.0 this is also allowed
    to be \code{"bzip2"} to specify that type of compression.}
  \item{refhook}{a hook function for handling reference objects.}
}
\details{
  Since these are internal, the file format is subject to change without
  notice.  The current  format is that of \code{\link{serialize}},
  compressed as if by \command{gzip} if \code{compress = FALSE}.
}

\value{
  For \code{.readRDS}, an \R object.

  For \code{.saveRDS}, \code{NULL} invisibly.
}
\keyword{internal}
