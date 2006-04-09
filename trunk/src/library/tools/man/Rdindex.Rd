\name{Rdindex}
\title{Generate Index from Rd Files}
\alias{Rdindex}
\description{
  Print a 2-column index table with \dQuote{names} and titles from given
  R documentation files to a given output file or connection.  The
  titles are nicely formatted between two column positions (typically 25
  and 72, respectively).
}
\usage{
Rdindex(RdFiles, outFile = "", type = NULL,
        width = 0.9 * getOption("width"), indent = NULL)
}
\arguments{
  \item{RdFiles}{a character vector specifying the Rd files to be used
    for creating the index, either by giving the paths to the files, or
    the path to a single directory with the sources of a package.}
  \item{outFile}{a connection, or a character string naming the output
    file to print to.  \code{""} (the default) indicates output is to
    the console.}
  \item{type}{a character string giving the documentation type of the Rd
    files to be included in the index, or \code{NULL} (the default).
    The type of an Rd file is typically specified via the
    \code{\\docType} tag; if \code{type} is \code{"data"}, Rd files
    whose \emph{only} keyword is \code{datasets} are included as well.}
  \item{width}{a positive integer giving the target column for wrapping
    lines in the output.}
  \item{indent}{a positive integer specifying the indentation of the
    second column.  Must not be greater than \code{width/2}, and
    defaults to \code{width/3}.}
}
\details{
  If a name is not a valid alias, the first alias (or the empty string
  if there is none) is used instead.
}
%% Currently no \example: at run time we do not have direct access to Rd
%% files.  What we could do is take an installed Rd archive, and extract
%% by splitting on the new-style \eof file separator, but we really want
%% a simple example, right?
\keyword{documentation}
\keyword{utilities}
