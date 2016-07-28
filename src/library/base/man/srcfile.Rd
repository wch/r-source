% File src/library/base/man/srcfile.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{srcfile}
\title{References to Source Files and Code}
\alias{srcfile}
\alias{srcfilecopy}
\alias{getSrcLines}
\alias{srcref}
\alias{srcfile-class}
\alias{srcfilecopy-class}
\alias{srcref-class}
\alias{print.srcfile}
\alias{summary.srcfile}
\alias{open.srcfile}
\alias{open.srcfilecopy}
\alias{close.srcfile}
\alias{print.srcref}
\alias{summary.srcref}
\alias{as.character.srcref}
\alias{.isOpen}
\alias{srcfilealias-class}
\alias{srcfilealias}
\alias{open.srcfilealias}
\alias{close.srcfilealias}

\description{
  These functions are for working with source files and more generally
  with \dQuote{source references} (\code{"srcref"}), i.e., references to
  source code.  The resulting data is used for printing and source level
  debugging, and is typically available in interactive \R sessions,
  namely when \code{\link{options}(keep.source = TRUE)}.
}
\usage{
srcfile(filename, encoding = getOption("encoding"), Enc = "unknown")
srcfilecopy(filename, lines, timestamp = Sys.time(), isFile = FALSE)
srcfilealias(filename, srcfile)
getSrcLines(srcfile, first, last)
srcref(srcfile, lloc)
\method{print}{srcfile}(x, \dots)
\method{summary}{srcfile}(object, \dots)
\method{open}{srcfile}(con, line, \dots)
\method{close}{srcfile}(con, \dots)
\method{print}{srcref}(x, useSource = TRUE, \dots)
\method{summary}{srcref}(object, useSource = FALSE, \dots)
\method{as.character}{srcref}(x, useSource = TRUE, to = x, \dots)
.isOpen(srcfile)
}
\arguments{
  \item{filename}{ The name of a file. }
  \item{encoding}{ The character encoding to assume for the file. }
  \item{Enc}{ The encoding with which to make strings: see the
    \code{encoding} argument of \code{\link{parse}}.}
  \item{lines}{ A character vector of source lines.  Other \R objects
    will be coerced to character. }
  \item{timestamp}{ The timestamp to use on a copy of a file. }
  \item{isFile}{ Is this \code{srcfilecopy} known to come from a file system file? }
  \item{srcfile}{ A \code{srcfile} object. }
  \item{first, last, line}{ Line numbers. }
  \item{lloc}{ A vector of four, six or eight values giving a source location; see
    \sQuote{Details}.}
  \item{x, object, con}{ An object of the appropriate class. }
  \item{useSource}{ Whether to read the \code{srcfile} to obtain the
    text of a \code{srcref}. }
  \item{to}{ An optional second \code{srcref} object to mark the end
    of the character range. }
  \item{\dots}{ Additional arguments to the methods; these will be ignored. }
}
\details{
  These functions and classes handle source code references.

  The \code{srcfile} function produces an object of class
  \code{srcfile}, which contains the name and directory of a source code
  file, along with its timestamp, for use in source level debugging (not
  yet implemented) and source echoing.  The encoding of the file is
  saved; see \code{\link{file}} for a discussion of encodings, and
  \code{\link{iconvlist}} for a list of allowable encodings on your platform.

  The \code{srcfilecopy} function produces an object of the descendant
  class \code{srcfilecopy}, which saves the source lines in a character
  vector.  It copies the value of the \code{isFile} argument, to help
  debuggers identify whether this text comes from a real file in the
  file system.

  The \code{srcfilealias} function produces an object of the descendant
  class \code{srcfilealias}, which gives an alternate name to another
  srcfile.  This is produced by the parser when a \code{#line} directive
  is used.

  The \code{getSrcLines} function reads the specified lines from
  \code{srcfile}.

  The \code{srcref} function produces an object of class
  \code{srcref}, which describes a range of characters in a
  \code{srcfile}.
  The \code{lloc} value gives the following values:
\preformatted{c(first_line, first_byte, last_line, last_byte, first_column,
  last_column, first_parsed, last_parsed)
}
  Bytes (elements 2, 4) and
  columns (elements 5, 6) may be different due to multibyte
  characters.  If only four values are given, the columns and bytes
  are assumed to match.  Lines (elements 1, 3) and parsed lines (
  elements 7, 8) may differ if a \code{#line} directive is used in
  code:  the former will respect the directive, the latter will just
  count lines.  If only 4 or 6 elements are given, the parsed lines
  will be assumed to match the lines.

  Methods are defined for \code{print}, \code{summary}, \code{open},
  and \code{close} for classes \code{srcfile} and \code{srcfilecopy}.
  The \code{open} method opens its internal \code{\link{file}} connection at
  a particular line; if it was already open, it will be repositioned
  to that line.

  Methods are defined for \code{print}, \code{summary} and
  \code{as.character} for class \code{srcref}.  The \code{as.character}
  method will read the associated source file to obtain the text
  corresponding to the reference.  If the \code{to} argument is given,
  it should be a second \code{srcref} that follows the first, in the
  same file; they will be treated as one reference to the whole
  range.  The exact behaviour depends on the
  class of the source file.  If the source file inherits from
  class \code{srcfilecopy}, the lines are taken from the saved copy
  using the \dQuote{parsed} line counts.  If not, an attempt
  is made to read the file, and the original line numbers of the
  \code{srcref} record (i.e., elements 1 and 3) are used.  If an error
  occurs (e.g., the file no longer exists), text like
  \code{<srcref: "file" chars 1:1 to 2:10>} will be returned instead,
  indicating the \code{line:column} ranges of the first and last
  character.  The \code{summary} method defaults to this type of
  display.

  Lists of \code{srcref} objects may be attached to expressions as the
  \code{"srcref"} attribute.  (The list of \code{srcref} objects should be the same
  length as the expression.)  By default, expressions are printed by
  \code{\link{print.default}} using the associated \code{srcref}.  To
  see deparsed code instead, call \code{\link{print}} with argument
  \code{useSource = FALSE}.  If a \code{srcref} object
  is printed with \code{useSource = FALSE}, the \code{<srcref: \dots>}
  record will be printed.

  \code{.isOpen} is intended for internal use:  it checks whether the
  connection associated with a \code{srcfile} object is open.
}
\value{
\code{srcfile} returns a \code{srcfile} object.

\code{srcfilecopy} returns a \code{srcfilecopy} object.

\code{getSrcLines} returns a character vector of source code lines.

\code{srcref} returns a \code{srcref} object.
}
\seealso{
  \code{\link{getSrcFilename}} for extracting information from a source
  reference, or \code{\link{removeSource}} to remove it from a
  (non-primitive) function (aka \sQuote{closure}).
}
\author{Duncan Murdoch}
\examples{\donttest{ # has timestamp
src <- srcfile(system.file("DESCRIPTION", package = "base"))
summary(src)
getSrcLines(src, 1, 4)
ref <- srcref(src, c(1, 1, 2, 1000))
ref
print(ref, useSource = FALSE)
}}

\keyword{ debugging }
\keyword{ utilities }
