\name{readLines}
\alias{readLines}
\title{Read Text Lines from a Connection}
\description{
  Read text lines from a connection.
}
\usage{
readLines(con = stdin(), n = -1, ok = TRUE)
}
\arguments{
  \item{con}{A connection object or a character string.}
  \item{n}{integer.  The (maximal) number of lines to
    read. Negative values indicate that one should read up to the end of
    the connection.}
  \item{ok}{logical. Is it OK to reach the end of the connection before
    \code{n > 0} lines are read? If not, an error will be generated.}
}
\details{
  If the \code{con} is a character string, the functions call
  \code{\link{file}} to obtain an file connection which is opened for
  the duration of the function call.

  If the connection is open it is read from its current position.
  If it is not open, it is opened for the duration of the call and then
  closed again.

  An incomplete last line (no final newline) will be accepted, with a warning.
}
\value{
  A character vector of length the number of lines read.
}
\seealso{
  \code{\link{connection}}, \code{\link{writeLines}},
  \code{\link{scan}}}

\examples{
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file="ex.data",
    sep="\n")
readLines("ex.data", n=-1)
unlink("ex.data") # tidy up
}
\keyword{file}
\keyword{connection}
