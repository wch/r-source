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
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
	    col.names = TRUE, qmethod = c("escape", "double"))
}
\arguments{
  \item{x}{the object to be written, typically a data frame.  If not, it
    is attempted to coerce \code{x} to a data frame.}
  \item{file}{either a character string naming a file or a connection.
    \code{""} indicates output to the console.}
  \item{append}{logical.  If \code{TRUE}, the output is appended to the
    file.  If \code{FALSE}, any existing file of the name is destroyed.}
  \item{quote}{a logical value or a numeric vector.  If \code{TRUE}, any
    character or factor columns will be surrounded by double quotes.  If
    a numeric vector, its elements are taken as the indices of the
    columns to quote.  In both cases, row and column names
    are quoted if they are written. If \code{FALSE}, nothing is quoted.}
  \item{sep}{the field separator string.  Values within each row of
    \code{x} are separated by this string.}
  \item{eol}{the character(s) to print at the end of each line (row).}
  \item{na}{the string to use for missing values in the data.}
  \item{dec}{the string to use for decimal points.}
  \item{row.names}{either a logical value indicating whether the row
    names of \code{x} are to be written along with \code{x}, or a
    character vector of row names to be written.}
  \item{col.names}{either a logical value indicating whether the column
    names of \code{x} are to be written along with \code{x}, or a
    character vector of column names to be written.}
  \item{qmethod}{a character string specifying how to deal with embedded
    double quote characters when quoting strings.  Must be one of
    \code{"escape"} (default), in which case the quote character is
    escaped in C style by a backslash, or \code{"double"}, in which case
    it is doubled.  You can specify just the initial letter.}
}
\details{
  Normally there is no column name for a column of row names.  If
  \code{col.names=NA} a blank column name is added.  This can be used to
  write CSV files for input to spreadsheets.

  \code{write.table} can be slow for data frames with large numbers
  (hundreds or more) of columns: this is inevitable as each column could
  be of a different class and so must be handled separately.
  Function \code{\link[MASS]{write.matrix}} in package \pkg{MASS}
  may be much more efficient if \code{x} is a matrix or can be
  represented in a numeric matrix.

  If the table has no columns the rownames will be written if
  \code{row.names=TRUE}, and vice versa.
}

\seealso{
  The \dQuote{R Data Import/Export} manual.

  \code{\link{read.table}}, \code{\link{write}}.

  \code{\link[MASS]{write.matrix}} in package \pkg{MASS}.
}

\examples{
\dontrun{
## To write a CSV file for input to Excel one might use
write.table(x, file = "foo.csv", sep = ",", col.names = NA)
## and to read this file back into R one needs
read.table("foo.csv", header = TRUE, sep = ",", row.names=1)
}
}
\keyword{print}
\keyword{file}
