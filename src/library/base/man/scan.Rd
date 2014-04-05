% File src/library/base/man/scan.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{scan}
\title{Read Data Values}
\alias{scan}
\description{
  Read data into a vector or list from the console or file.
}
\usage{
scan(file = "", what = double(), nmax = -1, n = -1, sep = "",
     quote = if(identical(sep, "\n")) "" else "'\"", dec = ".",
     skip = 0, nlines = 0, na.strings = "NA",
     flush = FALSE, fill = FALSE, strip.white = FALSE,
     quiet = FALSE, blank.lines.skip = TRUE, multi.line = TRUE,
     comment.char = "", allowEscapes = FALSE,
     fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)
}
\arguments{
  \item{file}{the name of a file to read data values from.  If the
    specified file is \code{""}, then input is taken from the keyboard
    (or whatever \code{\link{stdin}()} reads if input is redirected or
    \R is embedded).
    (In this case input can be terminated by a blank line or an EOF
    signal, \samp{Ctrl-D} on Unix and \samp{Ctrl-Z} on Windows.)

    Otherwise, the file name is interpreted \emph{relative} to the
    current working directory (given by \code{\link{getwd}()}),
    unless it specifies an \emph{absolute} path.
    Tilde-expansion is performed where supported.
    When running \R from a script, \code{file = "stdin"} can be used to
    refer to the process's \code{stdin} file stream.

    This can be a compressed file (see \code{\link{file}}).

    Alternatively, \code{file} can be a \code{\link{connection}},
    which will be opened if necessary, and if so closed at the end of
    the function call.  Whatever mode the connection is opened in,
    any of LF, CRLF or CR will be accepted as the EOL marker for a line
    and so will match \code{sep = "\n"}.

    \code{file} can also be a complete URL.  (For the supported URL
    schemes, see the \sQuote{URLs} section of the help for
    \code{\link{url}}.)

    To read a data file not in the current encoding (for example a
    Latin-1 file in a UTF-8 locale or conversely) use a
    \code{\link{file}} connection setting its \code{encoding} argument
    (or \code{scan}'s \code{fileEncoding} argument).
  }

  \item{what}{the type of \code{what} gives the type of data to be read.
    The supported types are \code{logical}, \code{integer}, \code{numeric},
    \code{complex}, \code{character}, \code{raw} and \code{\link{list}}.
    If \code{what} is a list, it is assumed that the lines of the data file
    are records each containing \code{length(what)} items
    (\sQuote{fields}) and the list components should have elements which
    are one of the first six types listed or \code{NULL}, see section
    \sQuote{Details} below.}

  \item{nmax}{integer: the maximum number of data values to be read, or if
    \code{what} is a list, the maximum number of records to be read.  If
    omitted or not positive or an invalid value for an integer
    (and \code{nlines} is not set to a positive value), \code{scan} will
    read to the end of \code{file}.}

  \item{n}{integer: the maximum number of data values to be read, defaulting to
    no limit.  Invalid values will be ignored.}

  \item{sep}{by default, scan expects to read \sQuote{white-space}
    delimited input fields.  Alternatively, \code{sep} can be used to
    specify a character which delimits fields.  A field is always
    delimited by an end-of-line marker unless it is quoted.

    If specified this should be the empty character string (the default)
    or \code{NULL} or a character string containing just one single-byte
    character.
  }

  \item{quote}{the set of quoting characters as a single character
    string or \code{NULL}.  In a multibyte locale the quoting characters
    must be ASCII (single-byte).}

  \item{dec}{decimal point character.  This should be a character string
    containing just one single-byte character.  (\code{NULL} and a
    zero-length character vector are also accepted, and taken as the
    default.)}

  \item{skip}{the number of  lines of the input file to skip before
    beginning to read data values.}

  \item{nlines}{if positive, the maximum number of lines of data to be read.}

  \item{na.strings}{character vector.  Elements of this vector are to be
    interpreted as missing (\code{\link{NA}}) values.  Blank fields are
    also considered to be missing values in logical, integer, numeric
    and complex fields.}

  \item{flush}{logical: if \code{TRUE}, \code{scan} will flush to the
    end of the line after reading the last of the fields requested.
    This allows putting comments after the last field, but precludes
    putting more that one record on a line.}

  \item{fill}{logical: if \code{TRUE}, \code{scan} will implicitly add
    empty fields to any lines with fewer fields than implied by
    \code{what}.}

  \item{strip.white}{vector of logical value(s) corresponding to items
    in the \code{what} argument.  It is used only when \code{sep} has
    been specified, and allows the stripping of leading and trailing
    \sQuote{white space} from \code{character} fields (\code{numeric} fields
    are always stripped).  Note: white space inside quoted strings is
    not stripped.

    If \code{strip.white} is of length 1, it applies to all fields;
    otherwise, if \code{strip.white[i]} is \code{TRUE} \emph{and} the
    \code{i}-th field is of mode character (because \code{what[i]} is)
    then the leading and trailing unquoted white space from field \code{i} is
    stripped.
  }

  \item{quiet}{logical: if \code{FALSE} (default), scan() will print a
    line, saying how many items have been read.}

  \item{blank.lines.skip}{logical: if \code{TRUE} blank lines in the
    input are ignored, except when counting \code{skip} and \code{nlines}.}

  \item{multi.line}{logical.  Only used if \code{what} is a list.  If
    \code{FALSE}, all of a record must appear on one line (but more than
    one record can appear on a single line).  Note that using \code{fill = TRUE}
    implies that a record will be terminated at the end of a line.}

  \item{comment.char}{character: a character vector of length one
    containing a single character or an empty string.  Use \code{""} to
    turn off the interpretation of comments altogether (the default).}

  \item{allowEscapes}{logical.  Should C-style escapes such as
    \samp{\\n} be processed (the default) or read verbatim?   Note that if
    not within quotes these could be interpreted as a delimiter (but not
    as a comment character).

    The escapes which are interpreted are the control characters
    \samp{\\a, \\b, \\f, \\n, \\r, \\t, \\v} and octal and
    hexadecimal representations like \samp{\\040} and \samp{\\0x2A}.  Any
    other escaped character is treated as itself, including backslash.
    Note that Unicode escapes (starting \samp{\\u} or \samp{\\U}: see
    \link{Quotes}) are never processed.
  }

  \item{fileEncoding}{character string: if non-empty declares the
    encoding used on a file (not a connection nor the keyboard) so the
    character data can be re-encoded.  See the \sQuote{Encoding} section
    of the help for \code{\link{file}}, and the \sQuote{R Data
      Import/Export Manual}.
  }

  \item{encoding}{encoding to be assumed for input strings.  If the
    value is \code{"latin1"} or \code{"UTF-8"} it is used to mark
    character strings as known to be in Latin-1 or UTF-8: it is not used
    to re-encode the input (see \code{fileEncoding}).
    See also \sQuote{Details}.
  }

  \item{text}{character string: if \code{file} is not supplied and this is,
    then data are read from the value of \code{text} via a text connection.
  }

  \item{skipNul}{logical: should nuls be skipped when reading character
    fields?}
}
\details{
  The value of \code{what} can be a list of types, in which case
  \code{scan} returns a list of vectors with the types given by the
  types of the elements in \code{what}.  This provides a way of reading
  columnar data.  If any of the types is \code{NULL}, the corresponding
  field is skipped (but a \code{NULL} component appears in the result).

  The type of \code{what} or its components can be one of the six
  atomic vector types or \code{NULL} (see \code{\link{is.atomic}}).

  \sQuote{White space} is defined for the purposes of this function as
  one or more contiguous characters from the set space, horizontal tab,
  carriage return and line feed.  It does not include form feed nor
  vertical tab, but in Latin-1 and Windows 8-bit locales 'space'
  includes non-breaking space.

  Empty numeric fields are always regarded as missing values.
  Empty character fields are scanned as empty character vectors, unless
  \code{na.strings} contains \code{""} when they are regarded as missing
  values.

  The allowed input for a numeric field is optional whitespace followed
  either \code{NA} or an optional sign followed by a decimal or
  hexadecimal constant (see \link{NumericConstants}), or \code{NaN},
  \code{Inf} or \code{infinity} (ignoring case).  Out-of-range values
  are recorded as \code{Inf}, \code{-Inf} or \code{0}.

  For an integer field the allowed input is optional whitespace,
  followed by either \code{NA} or an optional sign and one or more
  digits (\samp{0-9}): all out-of-range values are converted to
  \code{NA_integer_}.

  If \code{sep} is the default (\code{""}), the character \samp{\\}
  in a quoted string escapes the following character, so quotes may be
  included in the string by escaping them.

  If \code{sep} is non-default, the fields may be quoted in the style of
  \file{.csv} files where separators inside quotes (\code{''} or
  \code{""}) are ignored and quotes may be put inside strings by
  doubling them.  However, if \code{sep = "\\n"} it is assumed
  by default that one wants to read entire lines verbatim.

  Quoting is only interpreted in character fields and in \code{NULL}
  fields (which might be skipping character fields).

  Note that since \code{sep} is a separator and not a terminator,
  reading a file by \code{scan("foo", sep = "\\n", blank.lines.skip = FALSE)}
  will give an empty final line if the file ends in a linefeed and not if
  it does not.  This might not be what you expected; see also
  \code{\link{readLines}}.

  If \code{comment.char} occurs (except inside a quoted character
  field), it signals that the rest of the line should be regarded as a
  comment and be discarded.  Lines beginning with a comment character
  (possibly after white space with the default separator) are treated as
  blank lines.

  There is a line-length limit of 4095 bytes when reading from the
  console (which may impose a lower limit: see \sQuote{An Introduction
  to R}).

  There is a check for a user interrupt every 1000 lines if \code{what}
  is a list, otherwise every 10000 items.

  If \code{file} is a character string and \code{fileEncoding} is
  non-default, or if it is a not-already-open \link{connection} with a
  non-default \code{encoding} argument, the text is converted to UTF-8
  and declared as such (and the \code{encoding} argument to \code{scan}
  is ignored).  See the examples of \code{\link{readLines}}.

  Embedded nuls in the input stream will terminate the field currently
  being read, with a warning once per call to \code{scan}.  Setting
  \code{skipNul = TRUE} causes them to be ignored.
}
\value{
  if \code{what} is a list, a list of the same length and same names (as
  any) as \code{what}.

  Otherwise, a vector of the type of \code{what}.

  Character strings in the result will have a declared encoding if
  \code{encoding} is \code{"latin1"} or \code{"UTF-8"}.
}
\note{
  The default for \code{multi.line} differs from S.  To read one record
  per line, use \code{flush = TRUE} and \code{multi.line = FALSE}.
  (Note that quoted character strings can still include embedded newlines.)

  If number of items is not specified, the internal
  mechanism re-allocates memory in powers of two and so could use up
  to three times as much memory as needed.  (It needs both old and new
  copies.)  If you can, specify either \code{n} or \code{nmax} whenever
  inputting a large vector, and \code{nmax} or \code{nlines} when
  inputting a large list.

  Using \code{scan} on an open connection to read partial lines can lose
  chars: use an explicit separator to avoid this.

  Having \code{nul} bytes in fields (including \samp{\\0} if
  \code{allowEscapes = TRUE}) may lead to interpretation of the
  field being terminated at the \code{nul}.  They not normally present
  in text files -- see \code{\link{readBin}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{read.table}} for more user-friendly reading of data
  matrices;
  \code{\link{readLines}} to read a file a line at a time.
  \code{\link{write}}.

  \code{Quotes} for the details of C-style escape sequences.

  \code{\link{readChar}} and \code{\link{readBin}} to read fixed or
  variable length character strings or binary representations of numbers
  a few at a time from a connection.
}
\examples{
cat("TITLE extra line", "2 3 5 7", "11 13 17", file = "ex.data", sep = "\n")
pp <- scan("ex.data", skip = 1, quiet = TRUE)
scan("ex.data", skip = 1)
scan("ex.data", skip = 1, nlines = 1) # only 1 line after the skipped one
scan("ex.data", what = list("","","")) # flush is F -> read "7"
scan("ex.data", what = list("","",""), flush = TRUE)
unlink("ex.data") # tidy up

## "inline" usage
scan(text = "1 2 3")

}
\keyword{file}
\keyword{connection}
