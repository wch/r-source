\name{close.socket}
\title{Close a Socket}
\usage{
close.socket(socket, \dots)
}
\alias{close.socket}
\arguments{
  \item{socket}{A \code{socket} object}
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
}

\keyword{misc}
