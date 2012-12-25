% File src/library/utils/man/read.socket.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{read.socket}
\title{ Read from or Write to a Socket}
\usage{
read.socket(socket, maxlen = 256L, loop = FALSE)
write.socket(socket, string)
}
\alias{read.socket}
\alias{write.socket}
\arguments{
    \item{socket}{a socket object.}
    \item{maxlen}{maximum length (in bytes) of string to read.}
    \item{loop}{wait for ever if there is nothing to read?}
    \item{string}{string to write to socket.}
}
\description{
    \code{read.socket} reads a string from the specified socket,
    \code{write.socket} writes to the specified socket.  There is very
    little error checking done by either.
}
\value{
  \code{read.socket} returns the string read as a length-one character vector.

  \code{write.socket} returns the number of bytes written.

}
\author{Thomas Lumley}

\seealso{
    \code{\link{close.socket}}, \code{\link{make.socket}}
}

\examples{
finger <- function(user, host = "localhost", port = 79, print = TRUE)
{
    if (!is.character(user))
        stop("user name must be a string")
    user <- paste(user,"\r\n")
    socket <- make.socket(host, port)
    on.exit(close.socket(socket))
    write.socket(socket, user)
    output <- character(0)
    repeat{
        ss <- read.socket(socket)
        if (ss == "") break
        output <- paste(output, ss)
    }
    close.socket(socket)
    if (print) cat(output)
    invisible(output)
}
\dontrun{
finger("root")  ## only works if your site provides a finger daemon}
}
\keyword{misc}
