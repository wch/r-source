% File src/library/base/man/socketSelect.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{socketSelect}
\alias{socketSelect}
\title{Wait on Socket Connections}
\usage{
socketSelect(socklist, write = FALSE, timeout = NULL)
}
\arguments{
  \item{socklist}{list of open socket connections}
  \item{write}{logical.  If \code{TRUE} wait for corresponding socket to
               become available for writing; otherwise wait for it to become
               available for reading.}
  \item{timeout}{numeric or \code{NULL}. Time in seconds to wait for a
                 socket to become available; \code{NULL} means wait
                 indefinitely.}
}
\description{
  Waits for the first of several socket connections to become available.
}
\value{Logical the same length as \code{socklist} indicating
  whether the corresponding socket connection is available for
  output or input, depending on the corresponding value of \code{write}.
}
\details{
  The values in \code{write} are recycled if necessary to make up a
  logical vector the same length as \code{socklist}. Socket connections
  can appear more than once in \code{socklist}; this can be useful if
  you want to determine whether a socket is available for reading or
  writing.
}
\examples{
\dontrun{
## test whether socket connection s is available for writing or reading
socketSelect(list(s, s), c(TRUE, FALSE), timeout = 0)
}
}
\keyword{connection}
