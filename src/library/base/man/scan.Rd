\name{scan}
\title{Read Data Values}
\alias{scan}
\description{
  Read data into a vector or list from the console or file.
}
\usage{
scan(file = "", what = double(0), nmax = -1, n = -1, sep = "",
     quote = if (sep=="\n") "" else "'\"", dec = ".",
     skip = 0, nlines = 0, na.strings = "NA",
     flush = FALSE, fill = FALSE, strip.white = FALSE,
     quiet = FALSE, blank.lines.skip = TRUE, multi.line = TRUE,
     comment.char = "")
}
\arguments{
  \item{file}{the name of a file to read data values from.  If the
    specified file is \code{""}, then input is taken from the keyboard
    (or \code{stdin} if input is redirected).
    (In this case input can be terminated by a blank line or an EOF
    signal, \code{Ctrl-D} on Unix and \code{Ctrl-Z} on Windows.)

    Otherwise, the file name is interpreted \emph{relative} to the
    current working directory (given by \code{\link{getwd}()}),
    unless it specifies an \emph{absolute} path.
    Tilde-expansion is performed where supported.

    Alternatively, \code{file} can be a \code{\link{connection}},
    which will be opened if necessary, and if so closed at the end of
    the function call.  Whatever mode the connection is opened in,
    any of LF, CRLF or CR will be accepted as the EOL marker for a line
    and so will match \code{sep = "\n"}.

    \code{file} can also be a complete URL.
  }

  \item{what}{the type of \code{what} gives the type of data to be read.
    If \code{what} is a list, it is assumed that the lines of the data file
    are records each containing \code{length(what)} items (\dQuote{fields}).
    The supported types are \code{logical}, \code{integer}, \code{numeric},
    \code{complex}, \code{character}, \code{raw} and \code{list}:
    \code{list} values should have elements which are one of the first six
    types listed or \code{NULL}.}

  \item{nmax}{the maximum number of data values to be read, or if
    \code{what} is a list, the maximum number of records to be read.  If
    omitted or not positive (and \code{nlines} is not set to a
    positive value), \code{scan} will read to the end of \code{file}.}

  \item{n}{the maximum number of data values to be read, defaulting to
    no limit.}

  \item{sep}{by default, scan expects to read white-space delimited
    input fields.  Alternatively, \code{sep} can be used to specify a
    character which delimits fields.  A field is always delimited by an
    end-of-line marker unless it is quoted.}

  \item{quote}{the set of quoting characters as a single character
    string.}

  \item{dec}{decimal point character.}

  \item{skip}{the number of  lines of the input file to skip before
    beginning to read data values.}

  \item{nlines}{if positive, the maximum number of lines of data to be read.}

  \item{na.strings}{character vector.  Elements of this vector are to be
    interpreted as missing (\code{\link{NA}}) values.}

  \item{flush}{logical: if \code{TRUE}, \code{scan} will flush to the
    end of the line after reading the last of the fields requested.
    This allows putting comments after the last field, but precludes
    putting more that one record on a line.}

  \item{fill}{logical: if \code{TRUE}, \code{scan} will implicitly add
    empty fields to any lines with fewer fields than implied by
    \code{what}.}

  \item{strip.white}{vector of logical value(s) corresponding to items
    in the \code{what} argument. It is used only when \code{sep} has
    been specified, and allows the stripping of leading and trailing
    white space from \code{character} fields (\code{numeric} fields
    are always stripped).

    If \code{strip.white} is of length 1, it applies to all fields;
    otherwise, if \code{strip.white[i]} is \code{TRUE} \emph{and} the
    \code{i}-th field is of mode character (because \code{what[i]} is)
    then the leading and trailing white space from field \code{i} is
    stripped.}

  \item{quiet}{logical: if \code{FALSE} (default), scan() will print a
    line, saying how many items have been read.}

  \item{blank.lines.skip}{logical: if \code{TRUE} blank lines in the
    input are ignored, except when counting \code{skip} and \code{nlines}.}

  \item{multi.line}{logical. Only used if \code{what} is a list.
    If \code{FALSE}, all of a record must appear on one line (but more
    than one record can appear on a single line).  Note that using
    \code{fill = TRUE} implies that a record will terminated at the end
    of a line.}

  \item{comment.char}{character: a character vector of length one
    containing a single character or an empty string.  Use \code{""} to
    turn off the interpretation of comments altogether (the default).}
}
\details{
  The value of \code{what} can be a list of types, in which case
  \code{scan} returns a list of vectors with the types given by the
  types of the elements in \code{what}.  This provides a way of reading
  columnar data.  If any of the types is \code{NULL}, the corresponding
  field is skipped (but a \code{NULL} component appears in the result).

  The type of \code{what} or its components can be one of the five
  atomic types or \code{NULL},

  Empty numeric fields are always regarded as missing values.
  Empty character fields are scanned as empty character vectors, unless
  \code{na.strings} contains \code{""} when they are regarded as missing
  values.

  If \code{sep} is the default (\code{""}), the character \code{\\}
  in a quoted string escapes the following character, so quotes may be
  included in the string by escaping them.

  If \code{sep} is non-default, the fields may be quoted in the style of
  \file{.csv} files where separators inside quotes (\code{''} or
  \code{""}) are ignored and quotes may be put inside strings by
  doubling them.  However, if \code{sep = "\\n"} it is assumed
  by default that one wants to read entire lines verbatim.

  Quoting is only interpreted in character fields, and as from \R 1.8.0
  in \code{NULL} fields (which might be skipping character fields).

  Note that since \code{sep} is a separator and not a terminator,
  reading a file by \code{scan("foo", sep="\\n", blank.lines.skip=FALSE)}
  will give an empty file line if the file ends in a linefeed and not if
  it does not.  This might not be what you expected; see also
  \code{\link{readLines}}.

  If \code{comment.char} occurs (except inside a quoted character
  field), it signals that the rest of the line
  should be regarded as a comment and be discarded.  Lines beginning
  with a comment character (possibly after white space) are treated
  as blank lines.

  As from \R 2.1.0, \code{scan} attempts to share storage with
  character strings that have already been read.  If an upper bound on
  the number of character strings cannot be deduced from \code{nmax} or
  \code{n}, sharing is used for the first 10000 unique strings which are
  read in.
}
\value{
  if \code{what} is a list, a list of the same length and same names (as
  any) as \code{what}.

  Otherwise, a vector of the type of \code{what}.
}
\note{
  The default for \code{multi.line} differs from S.  To read one record
  per line, use \code{flush = TRUE} and \code{multi.line = FALSE}.

  If number of items is not specified, the internal
  mechanism re-allocates memory in powers of two and so could use up
  to three times as much memory as needed.  (It needs both old and new
  copies.)  If you can, specify either \code{n} or \code{nmax} whenever
  inputting a large vector, and \code{nmax} or \code{nlines} when
  inputting a large list.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{read.table}} for more user-friendly reading of data
  matrices;
  \code{\link{readLines}} to read a file a line at a time.
  \code{\link{write}}.
}
\examples{
cat("TITLE extra line", "2 3 5 7", "11 13 17", file="ex.data", sep="\n")
pp <- scan("ex.data", skip = 1, quiet= TRUE)
    scan("ex.data", skip = 1)
    scan("ex.data", skip = 1, nlines=1)# only 1 line after the skipped one
scan("ex.data", what = list("","","")) # flush is F -> read "7"
scan("ex.data", what = list("","",""), flush = TRUE)
unlink("ex.data") # tidy up
}
\keyword{file}
\keyword{connection}
