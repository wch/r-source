% File src/library/base/man/rawConnection.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
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

  If a connection is open for both input and output the initial raw
  vector supplied is copied when the connections is open
}

\value{
  For \code{rawConnection}, a connection object of class
  \code{"rawConnection"} which inherits from class \code{"connection"}.

  For \code{rawConnectionValue}, a raw vector.
}

\note{
  As output raw connections keep the internal raw vector up to date
  call-by-call, they are relatively expensive to use (although
  over-allocation is used), and it may be better to use an anonymous
  \code{\link{file}()} connection to collect output.

  On (rare) platforms where \code{vsnprintf} does not return the needed length
  of output there is a 100,000 character limit on the length of line for
  output connections: longer lines will be truncated with a warning.
}

\seealso{
  \code{\link{connections}}, \code{\link{showConnections}}.
}

\examples{
zz <- rawConnection(raw(0), "r+") # start with empty raw vector
writeBin(LETTERS, zz)
seek(zz, 0)
readLines(zz) # raw vector has embedded nuls
seek(zz, 0)
writeBin(letters[1:3], zz)
rawConnectionValue(zz)
close(zz)
}
\keyword{file}
\keyword{connection}
