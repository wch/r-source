\name{read.table}
\alias{read.table}
\alias{read.csv}
\alias{read.csv2}
\alias{read.delim}
\alias{read.delim2}
\title{Data Input}
\description{
  Reads a file in table format and creates a data frame from it, with
  cases corresponding to lines and variables to fields in the file.
}
\usage{
read.table(file, header = FALSE, sep = "", quote = "\"'", dec = ".",
           row.names, col.names, as.is = FALSE, na.strings = "NA",
           skip = 0, check.names = TRUE, fill = FALSE,
           strip.white = FALSE, blank.lines.skip = TRUE)

read.csv(file, header = TRUE, sep = ",", quote="\"", dec=".",
         fill = TRUE, \dots)

read.csv2(file, header = TRUE, sep = ";", quote="\"", dec=",",
         fill = TRUE, \dots)

read.delim(file, header = TRUE, sep = "\t", quote="\"", dec=".",
         fill = TRUE, \dots)

read.delim2(file, header = TRUE, sep = "\t", quote="\"", dec=",",
         fill = TRUE, \dots)
}
\arguments{
  \item{file}{the name of the file which the data are to be read from.
    Each row of the table appears as one line of the file.  If it does
    not contain an \emph{absolute} path, the file name is
    \emph{relative} to the current working directory,
    \code{\link{getwd}()}. Tilde-expansion is performed where supported.

    Alternatively, \code{file} can be a \code{\link{connection}}, which
    will be opened if necessary, and if so closed at the end of the
    function call.

    \code{file} can also be a complete URL.
  }

  \item{header}{a logical value indicating whether the file contains the
    names of the variables as its first line.  If missing, the value is
    determined from the file format: \code{header} is set to \code{TRUE}
    if and only if the first row contains one fewer field than the
    number of columns.}

  \item{sep}{the field separator character.  Values on each line of the
    file are separated by this character.  If \code{sep = ""} (the
    default for \code{read.table}) the separator is ``white space'',
    that is one or more spaces, tabs or newlines.}

  \item{quote}{the set of quoting characters. To disable quoting
    altogether, use \code{quote=""}.  See \code{\link{scan}} for the
    behaviour on quotes embedded in quotes.}

  \item{dec}{the character used in the file for decimal points.}

  \item{row.names}{a vector of row names.  This can be a vector giving
    the actual row names, or a single number giving the column of the
    table which contains the row names, or character string giving the
    name of the table column containing the row names.

    If there is a header and the first row contains one fewer field than
    the number of columns, the first column in the input is used for the
    row names.  Otherwise if \code{row.names} is missing, the rows are
    numbered.

    Using \code{row.names = NULL} forces row numbering.
  }

  \item{col.names}{a vector of optional names for the variables.
    The default is to use \code{"V"} followed by the column number.}

  \item{as.is}{the default behavior of \code{read.table} is to convert
    non-numeric variables to factors.  The variable \code{as.is}
    controls this conversion.  Its value is either a vector of logicals
    (values are recycled if necessary), or a vector of numeric indices
    which specify which columns should be left as character strings.}

  \item{na.strings}{a vector strings which are to be interpreted as
    \code{\link{NA}} values.}

  \item{skip}{the number of lines of the data file to skip before
    beginning to read data.}

  \item{check.names}{logical.  If \code{TRUE} then the names of the
    variables in the data frame are checked to ensure that they are
    syntactically valid variable names.  If necessary they are adjusted
    (by \code{\link{make.names}}) so that they are.}

  \item{fill}{logical. If \code{TRUE} then in case the rows have unequal
    length, blank fields are implicitly added.}

  \item{strip.white}{logical. Used only when \code{sep} has
    been specified, and allows the stripping of leading and trailing
    white space from \code{character} fields (\code{numeric} fields
    are always stripped).  See \code{\link{scan}} for further details,
    remembering that the columns may include the row names.}

  \item{blank.lines.skip}{logical: if \code{TRUE} blank lines in the
    input are ignored.}

  \item{\dots}{Further arguments to \code{read.table}.}
}
\value{
  A data frame (\code{\link{data.frame}}) containing a representation of
  the data in the file.  Empty input is an error unless \code{col.names}
  is specified, when a 0-row data frame is returned: similarly giving
  just a header line if \code{header = TRUE} results in a 0-row data frame.

  This function is the principal means of reading tabular data into \R.
}
\details{
  If \code{row.names} is not specified and the header line has one less
  entry than the number of columns, the first column is taken to be the
  row names.  This allows data frames to be read in from the format in
  which they are printed.  If  \code{row.names} is specified and does
  not refer to the first column, that column is discarded from such files.

  The number of data columns is determined by looking at the first five lines
  of input (or the whole file if it has less than five lines), or from
  the length of \code{col.names} if it is specified and
  is longer.  This could conceivably be wrong if \code{fill} or
  \code{blank.lines.skip} are true.
  
  \code{read.csv} and \code{read.csv2} are identical to
  \code{read.table} except for the defaults.  They are intended for
  reading ``comma separated value'' files (\file{.csv}) or the variant
  used in countries that use a comma as decimal point and a semicolon
  as field separator.  Similarly, \code{read.delim} and
  \code{read.delim2} are for reading delimited files, defaulting to the
  TAB character for the delimiter.  Notice that \code{header = TRUE} and
  \code{fill = TRUE} in these variants.
}
\note{
  The implementation of \code{read.table} currently reads everything as
  character using \code{\link{scan}} and subsequently defines
  \code{"numeric"} or \code{\link{factor}} variables.

  This is quite memory consuming for files of thousands of records and
  may need larger memory, see \code{\link{Memory}}.
}
\seealso{
  The `R Data Import/Export' manual.
  
  \code{\link{scan}},
  \code{\link{read.fwf}} for reading \emph{f}ixed \emph{w}idth
  \emph{f}ormatted input;
  \code{\link{read.table.url}} for ``reading'' data from the internet;
  \code{\link{write.table}};
  \code{\link{data.frame}}.

  \code{\link{count.fields}} can be useful to determine problems with
  reading files which result in reports of incorrect record lengths.
}
\keyword{file}
\keyword{connection}
