\name{writeLines}
\alias{writeLines}
\title{Write Lines to a Connection}
\description{
  Write text lines to a connection.
}
\usage{
writeLines(text, con = stdout(), sep = "\n")
}
\arguments{
  \item{text}{A character vector}
  \item{con}{A connection object or a character string.}
  \item{sep}{character. A string to be written to the connection after
    each line of text.}
}
\details{
  If the \code{con} is a character string, the functions call
  \code{\link{file}} to obtain an file connection which is opened for
  the duration of the function call.

  If the connection is open it is written from its current position.
  If it is not open, it is opened for the duration of the call and then
  closed again.
}
\seealso{\code{\link{connection}}, \code{\link{readLines}}, \code{\link{cat}}}

\keyword{file}
\keyword{connection}
