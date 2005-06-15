\name{write.table}
\alias{write.table}
\alias{write.table0}
\alias{write.csv}
\alias{write.csv2}
\title{Data Output}
\description{
  \code{write.table} prints its required argument \code{x} (after
  converting it to a data frame if it is not one nor a matrix) to
  a file or connection.
}
\usage{
write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
	    col.names = TRUE, qmethod = c("escape", "double"))

write.csv(\dots, col.names = NA, sep = ",", qmethod = "double")
write.csv2(\dots, col.names = NA, dec = ",", sep = ";", qmethod = "double")
}
\arguments{
  \item{x}{the object to be written, preferably a matrix or data frame.
    If not, it is attempted to coerce \code{x} to a data frame.}
  \item{file}{either a character string naming a file or a connection.
    \code{""} indicates output to the console.}
  \item{append}{logical.  If \code{TRUE}, the output is appended to the
    file.  If \code{FALSE}, any existing file of the name is destroyed.}
  \item{quote}{a logical value or a numeric vector.  If \code{TRUE}, any
    character or factor columns will be surrounded by double quotes.  If
    a numeric vector, its elements are taken as the indices of the
    columns to quote.  In both cases, row and column names
    are quoted if they are written.  If \code{FALSE}, nothing is quoted.}
  \item{sep}{the field separator string.  Values within each row of
    \code{x} are separated by this string.}
  \item{eol}{the character(s) to print at the end of each line (row).}
  \item{na}{the string to use for missing values in the data.}
  \item{dec}{the string to use for decimal points in numeric or complex
    columns: must be a single character.}
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

  \item{\dots}{further arguments to \code{write.table}.}
}
\details{
  By default there is no column name for a column of row names.  If
  \code{col.names = NA} and \code{row.names = TRUE} a blank column name
  is added.  This can be used to write CSV files for input to
  spreadsheets.  \code{write.csv} and \code{write.csv2} provide
  convenience wrappers for doing so.

  If the table has no columns the rownames will be written only if
  \code{row.names=TRUE}, and \emph{vice versa}.

  Real and complex numbers are written to the maximal possible precision.

  If a data frame has matrix-like columns these will be converted to
  multiple columns in the result (\emph{via} \code{\link{as.matrix}})
  and so a character \code{col.names} or a numeric \code{quote} should
  refer to the columns in the result, not the input.  Such matrix-like
  columns are unquoted by default.

  Any columns in a data frame which are lists or have a class
  (e.g. dates) will be converted by the appropriate \code{as.character}
  method: such columns are unquoted by default.  On the other hand,
  any class information for a matrix is discarded.

  The \code{dec} argument only applies to columns that are not subject
  to conversion to character because they have a class or are part of a
  matrix-like column, in particular to columns protected by
  \code{\link{I}()}.
}
\note{
  \code{write.table} can be slow for data frames with large numbers
  (hundreds or more) of columns: this is inevitable as each column could
  be of a different class and so must be handled separately.  If they
  are all of the same class, consider using a matrix instead.
}

\seealso{
  The \sQuote{R Data Import/Export} manual.

  \code{\link{read.table}}, \code{\link{write}}.

  \code{\link[MASS]{write.matrix}} in package \pkg{MASS}.
}

\examples{
\dontrun{
## To write a CSV file for input to Excel one might use
x <- data.frame(a = I("a \" quote"), b = pi)
write.table(x, file = "foo.csv", sep = ",", col.names = NA,
            qmethod = "double")
## and to read this file back into R one needs
read.table("foo.csv", header = TRUE, sep = ",", row.names = 1)
## NB: you do need to specify a separator if qmethod = "double".

### Alternatively
write.csv(x, file = "foo.csv")
read.csv("foo.csv", row.names = 1)
}}
\keyword{print}
\keyword{file}
