\name{showConnections}
\alias{showConnections}
\alias{getlConnection}
\alias{getAllConnections}
\alias{stdin}
\alias{stdout}
\alias{stderr}
\title{Display Connections}
\description{
  Display aspects of connections.
}
\usage{
showConnections(all=FALSE)
getConnection(what)

stdin()
stdout()
stderr()
}
\arguments{
  \item{all}{logical: if true all connections, including closed ones
    and the standard ones are displayed. If false only open user-creartd
    connections are included.}
  \item{what}{integer: a row number of the table given by
    \code{showConnections}.}
}
\details{
  \code{stdin()}, \code{stdout()} and \code{stderr()} are standard
  connections corresponding to input, output and error on the console
  respectively (and not necessarily to file streams).  They are text-mode
  connections of class \code{"terminal"} which cannot be opened or
  closed, and are read-only, write-only and write-only respectively.
  The \code{stdout()} connection can be re-directed by \code{\link{sink}}.

  \code{showConnections} returns a matrix of information.  If a
  connection object has been lost or forgotten, \code{getConnection}
  will take a row number from the table and return a connection object
  for that connection, which can be used to close the connection,
  for example.
}
\value{
  \code{stdin()}, \code{stdout()} and \code{stderr()} return connection
  objects.

  \code{showConnections} returns a character matrix of information with
  a row for each connection, by default only for open connections.

  \code{getConnection} returns a connection object, or \code{NULL}.
}
\seealso{\code{\link{connection}}}

\examples{
}
\keyword{connection}
