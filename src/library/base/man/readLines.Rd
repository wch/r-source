\name{readLines}
\alias{readLines}
\title{Read Text Lines from a File}
\description{
  Read text lines from a file.
}
\usage{
readLines(file, n = -1, ok = TRUE)
}
\arguments{
  \item{file}{A file name. Tilde-expansion will be performed if
    supported. \code{file = ""} is not allowed.
  }
  \item{n}{integer.  The (maximal) number of lines to
    read. Negative values indicate that one should read up to the end of
    the file.}
  \item{ok}{logical. Is it OK to reach the end of the file before
    \code{n > 0} lines are read? If not, an error will be generated.}
  \item{text}{A character vector.}
  \item{sep}{character. A string to be written to the file after
    each line of text.}
}
\value{
  A character vector of length the number of lines read.
}
\details{
  An incomplete last line (no final newline) will be accepted, with a warning.
}
\seealso{
  \code{\link{scan}}
}

\examples{
cat("TITLE extra line", "2 3 5 7", "", "11 13 17", file="ex.data",
    sep="\n")
readLines("ex.data", n=-1)
unlink("ex.data") # tidy up
}
\keyword{file}
