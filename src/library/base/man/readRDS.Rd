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
  \item{file}{a \link{connection} or the name of the file where the R object
    is saved to or read from.}
  \item{ascii}{a logical.  If \code{TRUE}, an ASCII representation is
    written; otherwise (default except for text-mode connections), a
    binary one is used.  See the commnents in the help for \code{\link{save}}.}
  \item{version}{the workspace format version to use.  \code{NULL}
    specifies the current default version (2).  Versions prior to 2 are not
    supported, so this will only be relevant when there are later versions.}
  \item{compress}{a logical specifying whether saving to a named file is
    to use compression.  Ignored when \code{file} is a connection.  As
    from \R 2.10.0 this is also allowed to be \code{"bzip2"} to specify
    that type of compression.}
  \item{refhook}{a hook function for handling reference objects.}
}
\details{
  Since these are internal, the file format is subject to change without
  notice.  The current format is that of \code{\link{serialize}},
  compressed as if by \command{gzip} if \code{compress = TRUE}.

  The current internal-only versions of \code{.readRDS} if given an
  un-opened connection wrap it in \code{\link{gzcon}} and
  \code{\link{close}} it after use: this was undocumented and will
  change in \R 2.13.0.
}

\value{
  For \code{.readRDS}, an \R object.

  For \code{.saveRDS}, \code{NULL} invisibly.
}
\keyword{internal}
