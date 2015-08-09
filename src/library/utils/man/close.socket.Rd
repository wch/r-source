% File src/library/utils/man/close.socket.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{close.socket}
\title{Close a Socket}
\usage{
close.socket(socket, \dots)
}
\alias{close.socket}
\arguments{
  \item{socket}{a \code{socket} object}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  Closes the socket and frees the space in the file descriptor table.  The
  port may not be freed immediately.
}
\value{
  logical indicating success or failure
}
\author{Thomas Lumley}
\seealso{
  \code{\link{make.socket}}, \code{\link{read.socket}}

  Compiling in support for sockets was optional prior to \R 3.3.0: see
  \code{\link{capabilities}("sockets")} to see if it is available.
}

\keyword{misc}
