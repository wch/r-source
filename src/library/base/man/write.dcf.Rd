\name{write.dcf}
\alias{write.dcf}
\title{Write Data in DCF Format}
\description{
  Writes an \R object to a file in Debian Control File format.
}
\usage{
write.dcf(x, file = "", append = FALSE,
          indent = 0.1 * getOption("width"),
          width = 0.9 * getOption("width"))
}
\arguments{
  \item{x}{the object to be written, typically a data frame.  If not, it
    is attempted to coerce \code{x} to a data frame.}
  \item{file}{either a character string naming a file or a connection.
    \code{""} indicates output to the console.}
  \item{append}{logical.  If \code{TRUE}, the output is appended to the
    file.  If \code{FALSE}, any existing file of the name is destroyed.}
  \item{indent}{a positive integer specifying the indentation for
    continuation lines in output entries.}
  \item{width}{a positive integer giving the target column for wrapping
    lines in the output.}
}
\seealso{
  \code{\link{parse.dcf}} for reading information from files in DCF
  format;
  \code{\link{write.table}}.
}
\examples{
## Create a reduced version of the `CONTENTS' file in package `eda'
x <- parse.dcf(file = system.file("CONTENTS", package = "eda"),
               fields = c("Entry", "Description"))
write.dcf(x)
}
\keyword{print}
\keyword{file}
