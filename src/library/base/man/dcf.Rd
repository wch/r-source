% File src/library/base/man/dcf.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2017 R Core Team
% Distributed under GPL 2 or later

\name{dcf}
\title{Read and Write Data in DCF Format}
\alias{read.dcf}
\alias{write.dcf}
\description{
  Reads or writes an \R object from/to a file in Debian Control File
  format.
}
\usage{
read.dcf(file, fields = NULL, all = FALSE, keep.white = NULL)

write.dcf(x, file = "", append = FALSE, useBytes = FALSE,
          indent = 0.1 * getOption("width"),
          width = 0.9 * getOption("width"),
          keep.white = NULL)
}
\arguments{
  \item{file}{either a character string naming a file or a \link{connection}.
    \code{""} indicates output to the console.  For \code{read.dcf} this
    can name a compressed file (see \code{\link{gzfile}}).}
  \item{fields}{a character vector with the names of the fields
    to read from the DCF file.  Default is to read all fields.}
  \item{all}{a logical indicating whether in case of multiple
    occurrences of a field in a record, all these should be gathered.
    If \code{all} is false (default), only the last such occurrence is
    used.}
  \item{keep.white}{a character vector with the names of the fields for
    which whitespace should be kept as is, or \code{NULL} (default)
    indicating that there are no such fields.  Coerced to character if
    possible.  For fields where whitespace is not to be kept as is,
    \code{read.dcf} removes leading and trailing whitespace, and
    \code{write.dcf} folds using \code{\link{strwrap}}.
   }
  \item{x}{the object to be written, typically a data frame.  If not, it
    is attempted to coerce \code{x} to a data frame.}
  \item{append}{logical.  If \code{TRUE}, the output is appended to the
    file.  If \code{FALSE}, any existing file of the name is destroyed.}
  \item{useBytes}{ignored, and retained only for back-compatibility.
    Since \code{write.dcf} now always writes UTF-8 (see \sQuote{Details}),
    the output is no longer re-encoded to the session's native encoding.}
  \item{indent}{a positive integer specifying the indentation for
    continuation lines in output entries.}
  \item{width}{a positive integer giving the target column for wrapping
    lines in the output.}
}
\details{
  DCF is a simple format for storing databases in plain text files that
  can easily be directly read and written by humans.  DCF is used in
  various places to store \R system information, like descriptions and
  contents of packages.

  The DCF rules as implemented in \R are:
  \enumerate{
    \item A database consists of one or more records, each with one or
    more named fields.  Not every record must contain each field.
    Fields may appear more than once in a record.
    \item Regular lines start with a non-whitespace character.
    \item Regular lines are of form \code{tag:value}, i.e., have a name
    tag and a value for the field, separated by \samp{:} (only the first
    \samp{:} counts).  The value can be empty (i.e., whitespace only).
    \item Lines starting with whitespace are continuation lines (to the
    preceding field) if at least one character in the line is
    non-whitespace.  Continuation lines where the only non-whitespace
    character is a \samp{.} are taken as blank lines (allowing for
    multi-paragraph field values).
    \item Records are separated by one or more empty (i.e., whitespace
    only) lines.
    \item Individual lines may not be arbitrarily long; prior to \R 3.0.2 the
    length limit was approximately 8191 bytes per line.
  }

  DCF files are required to be encoded in UTF-8.  \code{read.dcf}
  therefore returns values in UTF-8 (non-ASCII values being marked as
  such), and \code{write.dcf} converts values with a declared encoding
  (such as \code{"latin1"}) to UTF-8 and always writes UTF-8, regardless
  of the session's locale.  Any bytes which are not valid UTF-8 are not
  dropped but preserved visually by escaping them as \samp{<xx>} (where
  \code{xx} is the byte in hexadecimal), as done by
  \code{\link{iconv}(sub = "byte")}.  As a consequence, the
  \samp{Encoding} field of a \file{DESCRIPTION} file is no longer used to
  re-encode the other fields.

  \code{write.dcf} does not write \code{NA} fields.
}
\value{
  The default \code{read.dcf(all = FALSE)} returns a character matrix
  with one row per record and one column per field.  Leading and
  trailing whitespace of field values is ignored unless a field is
  listed in \code{keep.white}.  If a tag name is specified in the file,
  but the corresponding value is empty, then an empty string is
  returned.  If the tag name of a field is specified in \code{fields}
  but never used in a record, then the corresponding value is \code{NA}.
  If fields are repeated within a record, the last one encountered is
  returned.  Malformed lines lead to an error.

  For \code{read.dcf(all = TRUE)} a data frame is returned, again with
  one row per record and one column per field.  The columns are lists of
  character vectors for fields with multiple occurrences, and character
  vectors otherwise.

  Note that an empty \code{file} is a valid DCF file, and
  \code{read.dcf} will return a zero-row matrix or data frame.

  For \code{write.dcf}, invisible \code{NULL}.
}

\references{
  \url{https://www.debian.org/doc/debian-policy/ch-controlfields.html},
  which requires control files to be encoded in UTF-8.
}

\note{
  As from \R{} 3.4.0, \sQuote{whitespace} in all cases includes newlines.
}
\seealso{
  \code{\link{write.table}}.

  \code{\link{available.packages}}, which uses \code{read.dcf} to read
  the indices of package repositories.
}
\examples{\dontdiff{
## Create a reduced version of the DESCRIPTION file in package 'splines'
x <- read.dcf(file = system.file("DESCRIPTION", package = "splines"),
              fields = c("Package", "Version", "Title"))
write.dcf(x)
}
\donttest{## An online DCF file with multiple records
con <- url("https://cran.r-project.org/src/contrib/PACKAGES")
y <- read.dcf(con, all = TRUE)
close(con)
utils::str(y)
}}
\keyword{print}
\keyword{file}
