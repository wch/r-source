\name{pushBack}
\alias{pushBack}
\alias{pushBackLength}
\title{Push Text Back on to a Connection}
\description{
  Functions to push back text lines onto a connection, and to enquire
  how many lines are currently pushed back.
}
\usage{
pushBack(data, connection, newLine = TRUE)
pushBackLength(connection)
}
\arguments{
  \item{data}{a character vector.}
  \item{connection}{A connection.}
  \item{newLine}{logical. If true, a newline is appended to each string
    pushed back.}
}
\details{
  Several character strings can be pushed back on one or more occasions.
  The occasions form a stack, so the first line to be retrieved will be
  the first string from the last call to \code{pushBack}.  Lines which
  are pushed back are read prior to the normal input from the
  connection, by the normal text-reading functions such as
  \code{\link{readLines}} and \code{\link{scan}}.

  Pushback is only allowed for readable connections.

  Not all uses of connections respect pushbacks, in particular the input
  connection is still wired directly, so for example parsing
  commands from the console and \code{\link{scan}("")} ignore pushbacks on
  \code{\link{stdin}}.
}
\value{
  \code{pushBack} returns nothing.

  \code{pushBackLength} returns number of lines currently pushed back.
}
\seealso{\code{\link{connections}}, \code{\link{readLines}}.}

\examples{
zz <- textConnection(LETTERS)
readLines(zz, 2)
pushBack(c("aa", "bb"), zz)
pushBackLength(zz)
readLines(zz, 1)
pushBackLength(zz)
readLines(zz, 1)
readLines(zz, 1)
close(zz)
}
\keyword{connection}
