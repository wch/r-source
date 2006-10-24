\name{srcfile}
\alias{srcfile}
\alias{getSrcLines}
\alias{srcref}
\alias{srcfile-class}
\alias{srcref-class}
\alias{print.srcfile}
\alias{open.srcfile}
\alias{close.srcfile}
\alias{print.srcref}
\alias{as.character.srcref}
\alias{.isOpen}

\title{References to source files}
\description{
These functions are for working with source files.
}
\usage{
srcfile(filename)
getSrcLines(srcfile, first, last)
srcref(srcfile, lloc)
\method{print}{srcfile}(x, ...)
\method{open}{srcfile}(con, line, ...)
\method{close}{srcfile}(con, ...)
\method{print}{srcref}(x, useSource = TRUE, ...)
\method{as.character}{srcref}(srcref, useSource = TRUE)
.isOpen(srcfile)
}
\arguments{
  \item{filename}{ The name of a file }
  \item{srcfile}{ A \code{srcfile} object }
  \item{first, last, line}{ Line numbers }
  \item{lloc}{ A vector of four values giving a source location; see Details }
  \item{x, con}{ An object of the appropriate type }
  \item{srcref}{ A \code{srcref} object }
  \item{useSource}{ Whether to read the \code{srcfile} to obtain the text of a \code{srcref} }
  \item{...}{ Additional arguments to the methods; these will be ignored }
}
\details{
These functions and classes handle source code references.  

The \code{srcfile} function produces an object of class
\code{srcfile}, which contains the name and directory of a source code
file, along with its timestamp, for use in source level debugging (not
yet implemented).

The \code{getSrcLines} function reads the specified lines from
\code{srcfile}.

The \code{srcref} function produces an object of class \code{srcref},
which describes a range of characters in a \code{srcfile}.  The \code{lloc}
value gives the following values:  \code{c(first_line, first_column, last_line, last_column)}.

Methods are defined for \code{print}, \code{open}, and \code{close}
for class \code{srcfile}. The \code{open} method opens its internal
\code{file} connection at a particular line; if it was already open,
it will be repositioned to that line.

Methods are defined for \code{print} and \code{as.character} for class 
\code{srcref}.  The \code{as.character} method will read the associated
source file to obtain the text corresponding to the reference.  If an
error occurs (e.g. the file no longer exists), text like
\code{<srcref: "file" chars 1:1 to 2:10>} will be returned instead,
indicating the \code{line:column} ranges of the first and last character.

By default, several object types (functions, expressions, etc.) are printed by
\code{\link{print.default}} using the associated \code{srcref}.  To
see deparsed code instead, call \code{\link{print}} with argument
\code{useSource = FALSE}.  If the \code{srcref} attribute of an object
is printed with \code{useSource = FALSE}, the \code{<srcref: ...>}
record will be printed.

\code{.isOpen} is intended for internal use:  it checks whether the 
connection associated with a \code{srcfile} object is open.
}
\value{
\code{srcfile} returns a \code{srcfile} object.

\code{getSrcLines} returns a character vector of source code lines.

\code{srcref} returns a \code{srcref} object.
}
\author{Duncan Murdoch}

\examples{
src <- srcfile(system.file("DESCRIPTION", package = "base"))
getSrcLines(src, 1, 4)
ref <- srcref(src, c(1, 1, 2, 1000))
ref
print(ref, useSource = FALSE)
}

\keyword{ debugging }
\keyword{ utilities }
