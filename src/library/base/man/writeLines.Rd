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
  If the \code{con} is a character string, the function calls
  \code{\link{file}} to obtain a file connection which is opened for
  the duration of the function call.

  If the connection is open it is written from its current position.
  If it is not open, it is opened for the duration of the call and then
  closed again.

  Normally \code{writeLines} is used with a text connection, and the
  default separator is converted to the normal separator for that
  platform (LF on Unix/Linux, CRLF on Windows, CR on Classic MacOS).  For
  more control, open a binary connection and specify the precise value
  you want written to the file in \code{sep}.  For even more control,
  use \code{\link{writeChar}} on a binary connection.
}
\seealso{
  \code{\link{connections}}, \code{\link{writeChar}}, \code{\link{writeBin}},
  \code{\link{readLines}}, \code{\link{cat}}
}

\keyword{file}
\keyword{connection}
