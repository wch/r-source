% File src/library/utils/man/write.table.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{write.table}
\alias{write.table}
\alias{write.csv}
\alias{write.csv2}
\title{Data Output}
\description{
  \code{write.table} prints its required argument \code{x} (after
  converting it to a data frame if it is not one nor a matrix) to
  a file or \link{connection}.
}
\usage{
write.table(x, file = "", append = FALSE, quote = TRUE, sep = " ",
            eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")

write.csv(\dots)
write.csv2(\dots)
}
\arguments{
  \item{x}{the object to be written, preferably a matrix or data frame.
    If not, it is attempted to coerce \code{x} to a data frame.}
  \item{file}{either a character string naming a file or a \link{connection}
    open for writing.  \code{""} indicates output to the console.}
  \item{append}{logical. Only relevant if \code{file} is a character
    string.  If \code{TRUE}, the output is appended to the
    file.  If \code{FALSE}, any existing file of the name is destroyed.}
  \item{quote}{a logical value (\code{TRUE} or \code{FALSE}) or a
    numeric vector.  If \code{TRUE}, any character or factor columns
    will be surrounded by double quotes.  If a numeric vector, its
    elements are taken as the indices of columns to quote.  In both
    cases, row and column names are quoted if they are written.  If
    \code{FALSE}, nothing is quoted.}
  \item{sep}{the field separator string.  Values within each row of
    \code{x} are separated by this string.}
  \item{eol}{the character(s) to print at the end of each line (row).
    For example, \code{eol = "\r\n"} will produce Windows' line endings on
    a Unix-alike OS, and \code{eol = "\r"} will produce files as expected by
    Excel:mac 2004.}
  \item{na}{the string to use for missing values in the data.}
  \item{dec}{the string to use for decimal points in numeric or complex
    columns: must be a single character.}
  \item{row.names}{either a logical value indicating whether the row
    names of \code{x} are to be written along with \code{x}, or a
    character vector of row names to be written.}
  \item{col.names}{either a logical value indicating whether the column
    names of \code{x} are to be written along with \code{x}, or a
    character vector of column names to be written.  See the section on
    \sQuote{CSV files} for the meaning of \code{col.names = NA}.}
  \item{qmethod}{a character string specifying how to deal with embedded
    double quote characters when quoting strings.  Must be one of
    \code{"escape"} (default for \code{write.table}), in which case the
    quote character is escaped in C style by a backslash, or
    \code{"double"} (default for \code{write.csv} and
    \code{write.csv2}), in which case it is doubled.  You can specify
    just the initial letter.}
  \item{fileEncoding}{character string: if non-empty declares the
    encoding to be used on a file (not a connection) so the character data can
    be re-encoded as they are written.  See \code{\link{file}}.}

  \item{\dots}{arguments to \code{write.table}: \code{append},
    \code{col.names}, \code{sep}, \code{dec} and \code{qmethod}
    cannot be altered.
  }
}
\details{
  If the table has no columns the rownames will be written only if
  \code{row.names = TRUE}, and \emph{vice versa}.

  Real and complex numbers are written to the maximal possible precision.

  If a data frame has matrix-like columns these will be converted to
  multiple columns in the result (\emph{via} \code{\link{as.matrix}})
  and so a character \code{col.names} or a numeric \code{quote} should
  refer to the columns in the result, not the input.  Such matrix-like
  columns are unquoted by default.

  Any columns in a data frame which are lists or have a class
  (e.g., dates) will be converted by the appropriate \code{as.character}
  method: such columns are unquoted by default.  On the other hand,
  any class information for a matrix is discarded and non-atomic
  (e.g., list) matrices are coerced to character.

  Only columns which have been converted to character will be quoted if
  specified by \code{quote}.

  The \code{dec} argument only applies to columns that are not subject
  to conversion to character because they have a class or are part of a
  matrix-like column (or matrix), in particular to columns protected by
  \code{\link{I}()}.  Use \code{\link{options}("OutDec")} to control
  such conversions.

  In almost all cases the conversion of numeric quantities is governed
  by the option \code{"scipen"} (see \code{\link{options}}), but with
  the internal equivalent of \code{digits = 15}.  For finer control, use
  \code{\link{format}} to make a character matrix/data frame, and call
  \code{write.table} on that.

  These functions check for a user interrupt every 1000 lines of output.

  If \code{file} is a non-open connection, an attempt is made to open it
  and then close it after use.

  To write a Unix-style file on Windows, use a binary connection
  e.g.\sspace{}\code{file = file("filename", "wb")}.
}
\section{CSV files}{
  By default there is no column name for a column of row names.  If
  \code{col.names = NA} and \code{row.names = TRUE} a blank column name
  is added, which is the convention used for CSV files to be read by
  spreadsheets.  Note that such CSV files can be read in \R by
\preformatted{  read.csv(file = "<filename>", row.names = 1)}

  \code{write.csv} and \code{write.csv2} provide convenience wrappers
  for writing CSV files.  They set \code{sep} and \code{dec} (see
  below), \code{qmethod = "double"}, and \code{col.names} to \code{NA}
  if \code{row.names = TRUE} (the default) and to \code{TRUE} otherwise.

  \code{write.csv} uses \code{"."} for the decimal point and a comma for
  the separator.

  \code{write.csv2} uses a comma for the decimal point and a semicolon for
  the separator, the Excel convention for CSV files in some Western
  European locales.

  These wrappers are deliberately inflexible: they are designed to
  ensure that the correct conventions are used to write a valid file.
  Attempts to change \code{append}, \code{col.names}, \code{sep},
  \code{dec} or \code{qmethod} are ignored, with a warning.

  CSV files do not record an encoding, and this causes problems if they
  are not ASCII for many other applications.  Windows Excel 2007/10 will
  open files (e.g., by the file association mechanism) correctly if they
  are ASCII or UTF-16 (use \code{fileEncoding = "UTF-16LE"}) or perhaps
  in the current Windows codepage (e.g., \code{"CP1252"}), but the
  \sQuote{Text Import Wizard} (from the \sQuote{Data} tab) allows far
  more choice of encodings.  Excel:mac 2004/8 can \emph{import} only
  \sQuote{Macintosh} (which seems to mean Mac Roman), \sQuote{Windows}
  (perhaps Latin-1) and \sQuote{PC-8} files.  OpenOffice 3.x asks for
  the character set when opening the file.

  There is an IETF RFC4180 (\url{https://tools.ietf.org/html/rfc4180})
  for CSV files, which mandates comma as the separator and CRLF line
  endings.  \code{write.csv} writes compliant files on Windows: use
  \code{eol = "\r\n"} on other platforms.
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

  \code{\link[MASS]{write.matrix}} in package \CRANpkg{MASS}.
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
## or without row names
write.csv(x, file = "foo.csv", row.names = FALSE)
read.csv("foo.csv")

## To write a file in Mac Roman for simple use in Mac Excel 2004/8
write.csv(x, file = "foo.csv", fileEncoding = "macroman")
## or for Windows Excel 2007/10
write.csv(x, file = "foo.csv", fileEncoding = "UTF-16LE")
}}
\keyword{print}
\keyword{file}
