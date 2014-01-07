% File src/library/utils/man/read.table.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

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
read.table(file, header = FALSE, sep = "", quote = "\"'",
           dec = ".", row.names, col.names,
           as.is = !stringsAsFactors,
           na.strings = "NA", colClasses = NA, nrows = -1,
           skip = 0, check.names = TRUE, fill = !blank.lines.skip,
           strip.white = FALSE, blank.lines.skip = TRUE,
           comment.char = "#",
           allowEscapes = FALSE, flush = FALSE,
           stringsAsFactors = default.stringsAsFactors(),
           fileEncoding = "", encoding = "unknown", text)

read.csv(file, header = TRUE, sep = ",", quote = "\"",
         dec = ".", fill = TRUE, comment.char = "", \dots)

read.csv2(file, header = TRUE, sep = ";", quote = "\"",
          dec = ",", fill = TRUE, comment.char = "", \dots)

read.delim(file, header = TRUE, sep = "\t", quote = "\"",
           dec = ".", fill = TRUE, comment.char = "", \dots)

read.delim2(file, header = TRUE, sep = "\t", quote = "\"",
            dec = ",", fill = TRUE, comment.char = "", \dots)
}
\arguments{
  \item{file}{the name of the file which the data are to be read from.
    Each row of the table appears as one line of the file.  If it does
    not contain an \emph{absolute} path, the file name is
    \emph{relative} to the current working directory,
    \code{\link{getwd}()}. Tilde-expansion is performed where supported.
    This can be a compressed file (see \code{\link{file}}).

    Alternatively, \code{file} can be a readable text-mode
    \link{connection} (which will be opened for reading if
    necessary, and if so \code{\link{close}}d (and hence destroyed) at
    the end of the function call).  (If \code{\link{stdin}()} is used,
    the prompts for lines may be somewhat confusing.  Terminate input
    with a blank line or an EOF signal, \code{Ctrl-D} on Unix and
    \code{Ctrl-Z} on Windows.  Any pushback on \code{stdin()} will be
    cleared before return.)

    \code{file} can also be a complete URL.  (For the supported URL
    schemes, see the \sQuote{URLs} section of the help for
    \code{\link{url}}.)
  }

  \item{header}{a logical value indicating whether the file contains the
    names of the variables as its first line.  If missing, the value is
    determined from the file format: \code{header} is set to \code{TRUE}
    if and only if the first row contains one fewer field than the
    number of columns.}

  \item{sep}{the field separator character.  Values on each line of the
    file are separated by this character.  If \code{sep = ""} (the
    default for \code{read.table}) the separator is \sQuote{white space},
    that is one or more spaces, tabs, newlines or carriage returns.}

  \item{quote}{the set of quoting characters. To disable quoting
    altogether, use \code{quote = ""}.  See \code{\link{scan}} for the
    behaviour on quotes embedded in quotes.  Quoting is only considered
    for columns read as character, which is all of them unless
    \code{colClasses} is specified.}

  \item{dec}{the character used in the file for decimal points.}

  \item{row.names}{a vector of row names.  This can be a vector giving
    the actual row names, or a single number giving the column of the
    table which contains the row names, or character string giving the
    name of the table column containing the row names.

    If there is a header and the first row contains one fewer field than
    the number of columns, the first column in the input is used for the
    row names.  Otherwise if \code{row.names} is missing, the rows are
    numbered.

    Using \code{row.names = NULL} forces row numbering. Missing or
    \code{NULL} \code{row.names} generate row names that are considered
    to be \sQuote{automatic} (and not preserved by \code{\link{as.matrix}}).
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

    Possible values are \code{NA} (the default, when
    \code{\link{type.convert}} is used), \code{"NULL"} (when the column
    is skipped), one of the atomic vector classes (logical, integer,
    numeric, complex, character, raw), or \code{"factor"}, \code{"Date"}
    or \code{"POSIXct"}.  Otherwise there needs to be an \code{as}
    method (from package \pkg{methods}) for conversion from
    \code{"character"} to the specified formal class.

    Note that \code{colClasses} is specified per column (not per
    variable) and so includes the column of row names (if any).
  }

  \item{nrows}{integer: the maximum number of rows to read in.  Negative
    and other invalid values are ignored.}

  \item{skip}{integer: the number of lines of the data file to skip before
    beginning to read data.}

  \item{check.names}{logical.  If \code{TRUE} then the names of the
    variables in the data frame are checked to ensure that they are
    syntactically valid variable names.  If necessary they are adjusted
    (by \code{\link{make.names}}) so that they are, and also to ensure
    that there are no duplicates.}

  \item{fill}{logical. If \code{TRUE} then in case the rows have unequal
    length, blank fields are implicitly added.  See \sQuote{Details}.}

  \item{strip.white}{logical. Used only when \code{sep} has
    been specified, and allows the stripping of leading and trailing
    white space from unquoted \code{character} fields (\code{numeric} fields
    are always stripped).  See \code{\link{scan}} for further details
    (including the exact meaning of \sQuote{white space}),
    remembering that the columns may include the row names.}

  \item{blank.lines.skip}{logical: if \code{TRUE} blank lines in the
    input are ignored.}

  \item{comment.char}{character: a character vector of length one
    containing a single character or an empty string.  Use \code{""} to
    turn off the interpretation of comments altogether.}

  \item{allowEscapes}{logical.  Should C-style escapes such as
    \samp{\\n} be processed or read verbatim (the default)?   Note that if
    not within quotes these could be interpreted as a delimiter (but not
    as a comment character).  For more details see \code{\link{scan}}.}

  \item{flush}{logical: if \code{TRUE}, \code{scan} will flush to the
    end of the line after reading the last of the fields requested.
    This allows putting comments after the last field.}

  \item{stringsAsFactors}{logical: should character vectors be converted
    to factors?  Note that this is overridden by \code{as.is} and
    \code{colClasses}, both of which allow finer control.}

  \item{fileEncoding}{character string: if non-empty declares the
    encoding used on a file (not a connection) so the character data can
    be re-encoded.  See the \sQuote{Encoding} section of the help for
    \code{\link{file}}, the \sQuote{R Data Import/Export Manual} and
    \sQuote{Note}.
  }

  \item{encoding}{encoding to be assumed for input strings.  It is
    used to mark character strings as known to be in
    Latin-1 or UTF-8 (see \code{\link{Encoding}}): it is not used to
    re-encode the input, but allows \R to handle encoded strings in
    their native encoding (if one of those two).  See \sQuote{Value}.
  }

  \item{text}{character string: if \code{file} is not supplied and this is,
    then data are read from the value of \code{text} via a text connection.
    Notice that a literal string can be used to include (small) data sets
    within R code.
  }

  \item{\dots}{Further arguments to be passed to \code{read.table}.}
}

\details{
  This function is the principal means of reading tabular data into \R.

  Unless \code{colClasses} is specified, all columns are read as
  character columns and then converted using \code{\link{type.convert}}
  to logical, integer, numeric, complex or (depending on \code{as.is})
  factor as appropriate.  Quotes are (by default) interpreted in all
  fields, so a column of values like \code{"42"} will result in an
  integer column.

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
  \code{blank.lines.skip} are true, so specify \code{col.names} if
  necessary (as in the \sQuote{Examples}).

  \code{read.csv} and \code{read.csv2} are identical to
  \code{read.table} except for the defaults.  They are intended for
  reading \sQuote{comma separated value} files (\file{.csv}) or
  (\code{read.csv2}) the variant used in countries that use a comma as
  decimal point and a semicolon as field separator.  Similarly,
  \code{read.delim} and \code{read.delim2} are for reading delimited
  files, defaulting to the TAB character for the delimiter.  Notice that
  \code{header = TRUE} and \code{fill = TRUE} in these variants, and
  that the comment character is disabled.

  The rest of the line after a comment character is skipped; quotes
  are not processed in comments.  Complete comment lines are allowed
  provided \code{blank.lines.skip = TRUE}; however, comment lines prior
  to the header must have the comment character in the first non-blank
  column.

  Quoted fields with embedded newlines are supported except after a
  comment character.  On the other hand, embedded nuls in the input
  stream are not supported, and in some cases will give a warning and in
  all cases terminate the line or field.
}

\value{
  A data frame (\code{\link{data.frame}}) containing a representation of
  the data in the file.

  Empty input is an error unless \code{col.names} is specified, when a
  0-row data frame is returned: similarly giving just a header line if
  \code{header = TRUE} results in a 0-row data frame.  Note that in
  either case the columns will be logical unless \code{colClasses} was
  supplied.

  Character strings in the result (including factor levels) will have a
  declared encoding if \code{encoding} is \code{"latin1"} or
  \code{"UTF-8"}.
}

\section{Memory usage}{
  These functions can use a surprising amount of memory when reading
  large files.  There is extensive discussion in the \sQuote{R Data
  Import/Export} manual, supplementing the notes here.

  Less memory will be used if \code{colClasses} is specified as one of
  the six \link{atomic} vector classes.  This can be particularly so when
  reading a column that takes many distinct numeric values, as storing
  each distinct value as a character string can take up to 14 times as
  much memory as storing it as an integer.

  Using \code{nrows}, even as a mild over-estimate, will help memory
  usage.

  Using \code{comment.char = ""} will be appreciably faster than the
  \code{read.table} default.

  \code{read.table} is not the right tool for reading large matrices,
  especially those with many columns: it is designed to read
  \emph{data frames} which may have columns of very different classes.
  Use \code{\link{scan}} instead for matrices.
}

\note{
  The columns referred to in \code{as.is} and \code{colClasses} include
  the column of row names (if any).

  Because this function uses \code{\link{pushBack}} it can only handle
  character strings which can be represented in the current locale.  So
  although \code{fileEncoding} can be used to specify the encoding of
  the input file (or a connection can be specified which re-encodes),
  the implied re-encoding must be possible.  This is not a problem in
  UTF-8 locales, but it can be on Windows --- \code{\link{readLines}} or
  \code{\link{scan}} can be used to avoid this limitation since they have
  special provisions to convert input to UTF-8.
}

\seealso{
  The \sQuote{R Data Import/Export} manual.

  \code{\link{scan}}, \code{\link{type.convert}},
  \code{\link{read.fwf}} for reading \emph{f}ixed \emph{w}idth
  \emph{f}ormatted input;
  \code{\link{write.table}};
  \code{\link{data.frame}}.

  \code{\link{count.fields}} can be useful to determine problems with
  reading files which result in reports of incorrect record lengths (see
  the \sQuote{Examples} below).

  \url{http://tools.ietf.org/html/rfc4180} for the IANA definition of
  CSV files (which requires comma as separator and CRLF line endings).
}

\references{
  Chambers, J. M. (1992)
  \emph{Data for models.}
  Chapter 3 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth & Brooks/Cole.
}

\examples{
## using count.fields to handle unknown maximum number of fields
## when fill = TRUE
test1 <- c(1:5, "6,7", "8,9,10")
tf <- tempfile()
writeLines(test1, tf)

read.csv(tf, fill = TRUE) # 1 column
ncol <- max(count.fields(tf, sep = ","))
read.csv(tf, fill = TRUE, header = FALSE,
         col.names = paste0("V", seq_len(ncol)))
unlink(tf)

## "Inline" data set, using text=
## Notice that leading and trailing empty lines are auto-trimmed

read.table(header = TRUE, text = "
a b
1 2
3 4
")
}


\keyword{file}
\keyword{connection}
