\name{dcf}
\alias{read.dcf}
\alias{write.dcf}
\title{Read and Write Data in DCF Format}
\description{
  Reads or writes an \R object from/to a file in Debian Control File format.
}
\usage{
read.dcf(file, fields=NULL)
write.dcf(x, file = "", append = FALSE,
          indent = 0.1 * getOption("width"),
          width = 0.9 * getOption("width"))
}
\arguments{
  \item{file}{either a character string naming a file or a connection.
    \code{""} indicates output to the console.}
  \item{fields}{Fields to read from the DCF file. Default is to read all
    fields.}
  \item{x}{the object to be written, typically a data frame.  If not, it
    is attempted to coerce \code{x} to a data frame.}
  \item{append}{logical.  If \code{TRUE}, the output is appended to the
    file.  If \code{FALSE}, any existing file of the name is destroyed.}
  \item{indent}{a positive integer specifying the indentation for
    continuation lines in output entries.}
  \item{width}{a positive integer giving the target column for wrapping
    lines in the output.}
}
\seealso{
  \code{\link{write.table}}.
}
\examples{
## Create a reduced version of the `CONTENTS' file in package `eda'
x <- read.dcf(file = system.file("CONTENTS", package = "eda"),
               fields = c("Entry", "Description"))
write.dcf(x)
}
\keyword{print}
\keyword{file}
