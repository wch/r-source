% File src/library/base/man/rawConnection.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later

\name{rawConnection}
\alias{rawConnection}
\alias{rawConnectionValue}
\title{Raw Connections}
\description{
  Input and output raw connections.
}
\usage{
rawConnection(object, open = "r")

rawConnectionValue(con)
}
\arguments{
  \item{object}{character or raw vector.  A description of the connection. 
    For an input this is an \R raw vector object, and for an output
    connection the name for the connection.
  }
  \item{open}{character.  Any of the standard connection open modes.}
  \item{con}{An output raw connection.}
}
\details{
  An input raw connection is opened and the raw vector is copied
  at the time the connection object is created, and \code{close}
  destroys the copy.

  An output raw connection is opened and creates an \R raw vector
  internally.  The raw vector can be retrieved \emph{via} 
  \code{rawConnectionValue}.

}

\value{
  For \code{rawConnection}, a connection object of class
  \code{"rawConnection"} which inherits from class \code{"connection"}.

  For \code{rawConnectionValue}, a raw vector.
}

\note{
  As output text connections keep the character vector up to date
  line-by-line, they are relatively expensive to use, and it is often
  better to use an anonymous \code{\link{file}()} connection to collect
  output.

  On platforms where \code{vsnprintf} does not return the needed length
  of output (e.g., Windows) there is a 100,000 character limit on the
  length of line for output connections: longer lines will be truncated
  with a warning.
}
 
\seealso{
  \code{\link{connections}}, \code{\link{showConnections}}.
}

\examples{
zz <- rawConnection(raw(0), "r+")
writeBin(LETTERS, zz)
seek(zz, 0)
readLines(zz)
seek(zz, 0)
writeBin(letters[1:3], zz)
rawConnectionValue(zz)
close(zz)
}
\keyword{file}
\keyword{connection}
