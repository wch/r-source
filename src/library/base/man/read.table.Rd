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
           colClasses = NA, nrows = -1,
           skip = 0, check.names = TRUE, fill = !blank.lines.skip,
           strip.white = FALSE, blank.lines.skip = TRUE,
           comment.char = "#", allowEscapes = FALSE)

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
    function call.  (If \code{stdin()} is used, the prompts for lines
    may be somewhat confusing.  Terminate input with a blank line or an EOF
    signal, \code{Ctrl-D} on Unix and \code{Ctrl-Z} on Windows.  Any
    pushback on \code{stdin()} will be cleared before return.)

    \code{file} can also be a complete URL.

    To read a data file not in the current encoding (for example a
    Latin-1 file in a UTF-8 locale or conversely) use a
    \code{\link{file}} connection setting the \code{encoding} argument.
  }

  \item{header}{a logical value indicating whether the file contains the
    names of the variables as its first line.  If missing, the value is
    determined from the file format: \code{header} is set to \code{TRUE}
    if and only if the first row contains one fewer field than the
    number of columns.}

  \item{sep}{the field separator character.  Values on each line of the
    file are separated by this character.  If \code{sep = ""} (the
    default for \code{read.table}) the separator is \dQuote{white space},
    that is one or more spaces, tabs, newlines or carriage returns.}

  \item{quote}{the set of quoting characters. To disable quoting
    altogether, use \code{quote = ""}.  See \code{\link{scan}} for the
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
    character variables (which are not converted to logical, numeric or
    complex) to factors.  The variable \code{as.is} controls the
    conversion of columns not otherwise specified by \code{colClasses}.
    Its value is either a vector of logicals (values are recycled if
    necessary), or a vector of numeric or character indices which
    specify which columns should not be converted to factors.

    Note: to suppress all conversions including those of numeric
    columns, set \code{colClasses = "character"}.

    Note that \code{as.is} is specified per column (not per
    variable) and so includes the column of row names (if any) and any
    columns to be skipped.
  }

  \item{na.strings}{a character vector of strings which are to be
    interpreted as \code{\link{NA}} values.  Blank fields are also
    considered to be missing values in logical, integer, numeric and
    complex fields.}

  \item{colClasses}{character.  A vector of classes to be assumed for
    the columns.  Recycled as necessary, or if the character vector is
    named, unspecified values are taken to be \code{NA}.

    Possible values are \code{NA} (when \code{\link{type.convert}} is
    used), \code{"NULL"} (when the column is skipped), one of the atomic
    vector classes (logical, integer, numeric, complex, character, raw),
    or \code{"factor"}, \code{"Date"} or \code{"POSIXct"}.  Otherwise
    there needs to be an \code{as} method (from package \pkg{methods})
    for conversion from \code{"character"} to the specified formal
    class.
    
    Note that \code{colClasses} is specified per column (not per
    variable) and so includes the column of row names (if any).
  }

  \item{nrows}{the maximum number of rows to read in.  Negative values
    are ignored.}

  \item{skip}{the number of lines of the data file to skip before
    beginning to read data.}

  \item{check.names}{logical.  If \code{TRUE} then the names of the
    variables in the data frame are checked to ensure that they are
    syntactically valid variable names.  If necessary they are adjusted
    (by \code{\link{make.names}}) so that they are, and also to ensure
    that there are no duplicates.}

  \item{fill}{logical. If \code{TRUE} then in case the rows have unequal
    length, blank fields are implicitly added.  See Details.}

  \item{strip.white}{logical. Used only when \code{sep} has
    been specified, and allows the stripping of leading and trailing
    white space from \code{character} fields (\code{numeric} fields
    are always stripped).  See \code{\link{scan}} for further details,
    remembering that the columns may include the row names.}

  \item{blank.lines.skip}{logical: if \code{TRUE} blank lines in the
    input are ignored.}
  
  \item{comment.char}{character: a character vector of length one
    containing a single character or an empty string.  Use \code{""} to
    turn off the interpretation of comments altogether.}

  \item{allowEscapes}{logical.  Should C-style escapes such as
    \code{\n} be processed or read verbatim (the default)?   Note that if
    not within quotes these could be interpreted as a delimiter (but not
    as a comment character).  For more details see \code{\link{scan}}.}

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
  A field or line is \sQuote{blank} if it contains nothing (except
  whitespace if no separator is specified) before a comment character or
  the end of the field or line.
  
  If \code{row.names} is not specified and the header line has one less
  entry than the number of columns, the first column is taken to be the
  row names.  This allows data frames to be read in from the format in
  which they are printed.  If \code{row.names} is specified and does
  not refer to the first column, that column is discarded from such files.

  The number of data columns is determined by looking at the first five lines
  of input (or the whole file if it has less than five lines), or from
  the length of \code{col.names} if it is specified and
  is longer.  This could conceivably be wrong if \code{fill} or
  \code{blank.lines.skip} are true, so specify \code{col.names} if necessary.
  
  \code{read.csv} and \code{read.csv2} are identical to
  \code{read.table} except for the defaults.  They are intended for
  reading \dQuote{comma separated value} files (\file{.csv}) or
  (\code{read.csv2}) the variant
  used in countries that use a comma as decimal point and a semicolon
  as field separator.  Similarly, \code{read.delim} and
  \code{read.delim2} are for reading delimited files, defaulting to the
  TAB character for the delimiter.  Notice that \code{header = TRUE} and
  \code{fill = TRUE} in these variants.

  The rest of the line after a comment character is skipped; quotes
  are not processed in comments.  Complete comment lines are allowed
  provided \code{blank.lines.skip = TRUE}; however, comment lines prior
  to the header must have the comment character in the first non-blank
  column.

  As from \R 1.9.0 quoted fields with embedded newlines are supported
  except after a comment character.
}
\note{
  The columns referred to in \code{as.is} and \code{colClasses} include
  the column of row names (if any).
  
  Less memory will be used if \code{colClasses} is specified as one of
  the six atomic vector classes.

  Using \code{nrows}, even as a mild over-estimate, will help memory
  usage.

  Using \code{comment.char = ""} will be appreciably faster.

  \code{read.table} is not the right tool for reading large matrices,
  especially those with many columns: it is designed to read
  \emph{data frames} which may have columns of very different classes.
  Use \code{\link{scan}} instead.

  Prior to version 1.9.0, underscores were not valid in variable names,
  and code that relies on them being converted to dots will no longer
  work. The simplest workaround is to use 
   \code{names(d) <- gsub("_", ".", names(d))},
  or, avoiding the (small) risk of creating duplicate names,
   \code{names(d) <- make.names(gsub("_", ".", names(d)), unique=TRUE)}. 
}

\seealso{
  The \emph{R Data Import/Export} manual.
  
  \code{\link{scan}}, \code{\link{type.convert}},
  \code{\link{read.fwf}} for reading \emph{f}ixed \emph{w}idth
  \emph{f}ormatted input;
  \code{\link{write.table}};
  \code{\link{data.frame}}.

  \code{\link{count.fields}} can be useful to determine problems with
  reading files which result in reports of incorrect record lengths.
}
\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\keyword{file}
\keyword{connection}
