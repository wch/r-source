\name{close.socket}
\title{Close a Socket}
\usage{
close.socket(socket)
}
\alias{close.socket}
\arguments{
    \item{socket}{A \code{socket} object}
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
