\name{count.fields}
\title{Count the Number of Fields per Line}
\usage{
count.fields(file, sep = "", quote = "", skip = 0)
}
\alias{count.fields}
\arguments{
  \item{file}{a character string naming an ASCII data file}
  \item{sep}{the field separator character.  Values on each line of the
    file are separated by this character.  By default, arbitrary amounts
    of whitespace can separate fields.}
  \item{quote}{the set of quoting characters}
  \item{skip}{the number of lines of the data file to skip before
    beginning to read data.}
}
\description{
  \code{count.fields} counts the number of fields, as separated by
  \code{sep}, in each of the lines of \code{file} read.  It is used by
  \code{\link{read.table}};  a user will typically have no need for it.
}
\value{
  A vector with the numbers of fields found.
}
\seealso{
  \code{\link{read.table}}
}
\examples{
cat("NAME", "1:John", "2:Paul", file = "foo", sep = "\n")
count.fields("foo", sep = ":")
}
\keyword{file}
