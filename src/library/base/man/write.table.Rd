\name{write.table}
\alias{write.table}
\title{Data Output}
\description{
  \code{write.table} prints its required argument \code{x} (after
  converting it to a data frame if it is not one already) to
  \code{file}.  The entries in each line (row) are separated by the
  value of \code{sep}.
}
\usage{
write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = NA, row.names = TRUE, col.names = TRUE)
}
\arguments{
  \item{x}{the object to be written, typically a data frame.  If not, it
    is attempted to create one from it.}
  \item{file}{the name of the file which the data are to be written to.}
  \item{sep}{the field separator string.  Values on each line of the file
    are separated by this string.} 
  \item{col.names}{a logical value indicating whether the column names
    of \code{x} are to be written along with \code{x}, or a character
    vector of column names to be written.}
  \item{row.names}{a logical value indicating whether the row names of
    \code{x} are to be written along with \code{x}, or a character
    vector of row names to be written.}
  \item{quote}{a logical or a numeric vector.  If \code{TRUE}, any
    strings in the data will be surrounded by double quotes.  If a
    numeric vector, its elements are taken as the indices of the
    variable (columns) to quote.  In both cases, row and columns names
    are always quoted if they are written.} 
  \item{na}{the string to use for missing values in the data.}
  \item{eol}{the character(s) to print at the end of each line (row).}
}
\seealso{
  \code{\link{read.table}}.
}
\keyword{print}
\keyword{file}
