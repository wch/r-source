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

  If the final line is incomplete (no final EOL marker) the behaviour
  depends on whether the connection is blocking or not. For a blocking
  text-mode connection (or a non-text-mode connection) the line
  will be accepted, with a warning.  For a non-blocking text-mode
  connection the incomplete line is pushed back, silently.
}
\value{
  A character vector of length the number of lines read.
}
\seealso{
  \code{\link{connections}}, \code{\link{writeLines}}, \code{\link{readBin}},
  \code{\link{scan}}}

\examples{
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file="ex.data",
    sep="\n")
readLines("ex.data", n=-1)
unlink("ex.data") # tidy up

## difference in blocking
cat("123\nabc", file = "test1")
readLines("test1") # line with a warning

con <- file("test1", "r", blocking = FALSE)
readLines(con) # empty
cat(" def\n", file = "test1", append = TRUE)
readLines(con) # gets both
close(con)

unlink("test1") # tidy up
}
\keyword{file}
\keyword{connection}
