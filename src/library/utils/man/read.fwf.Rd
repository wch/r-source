% File src/library/utils/man/read.fwf.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{read.fwf}
\alias{read.fwf}
\title{Read Fixed Width Format Files}
\description{
  Read a table of \bold{f}ixed \bold{w}idth \bold{f}ormatted
  data into a \code{\link{data.frame}}.
}
\usage{
read.fwf(file, widths, header = FALSE, sep = "\t",
         skip = 0, row.names, col.names, n = -1,
         buffersize = 2000, fileEncoding = "", \dots)
}
\arguments{
  \item{file}{
    the name of the file which the data are to be read from.

    Alternatively, \code{file} can be a \link{connection}, which
    will be opened if necessary, and if so closed at the end of the
    function call.
  }
  \item{widths}{integer vector, giving the widths of the fixed-width
    fields (of one line), or list of integer vectors giving widths for
    multiline records.}
  \item{header}{a logical value indicating whether the file contains the
    names of the variables as its first line.  If present, the names
    must be delimited by \code{sep}.}
  \item{sep}{character; the separator used internally; should be a
    character that does not occur in the file (except in the header).}
  \item{skip}{number of initial lines to skip; see
    \code{\link{read.table}}.}
  \item{row.names}{see \code{\link{read.table}}.}
  \item{col.names}{see \code{\link{read.table}}.}
  \item{n}{the maximum number of records (lines) to be read, defaulting
    to no limit.}
  \item{buffersize}{Maximum number of lines to read at one time}
  
  \item{fileEncoding}{character string: if non-empty declares the
    encoding used on a file (not a connection) so the character data can
    be re-encoded.  See the \sQuote{Encoding} section of the help for
    \code{\link{file}}, the \sQuote{R Data Import/Export Manual} and
    \sQuote{Note}.}
  
  \item{\dots}{further arguments to be passed to
    \code{\link{read.table}}.  Useful such arguments include
    \code{as.is}, \code{na.strings}, \code{colClasses} and \code{strip.white}.}
}
% PR#8083 mentions strip.white
\value{
  A \code{\link{data.frame}} as produced by \code{\link{read.table}}
  which is called internally.
}
\details{
  Multiline records are concatenated to a single line before processing.
  Fields that are of zero-width or are wholly beyond the end of the line
  in \code{file} are replaced by \code{NA}.

  Negative-width fields are used to indicate columns to be skipped, e.g.,
  \code{-5} to skip 5 columns.  These fields are not seen by
  \code{read.table} and so should not be included in a \code{col.names}
  or \code{colClasses} argument (nor in the header line, if present).

  Reducing the \code{buffersize} argument may reduce memory use when
  reading large files with long lines.  Increasing \code{buffersize} may
  result in faster processing when enough memory is available.
  
  Note that \code{read.fwf} (not \code{read.table}) reads the supplied
  file, so the latter's argument \code{encoding} will not be useful.
}
\author{
  Brian Ripley for \R version: originally in \code{Perl} by Kurt Hornik.
}
\seealso{
  \code{\link{scan}} and \code{\link{read.table}}.
  
  \code{\link{read.fortran}} for another style of fixed-format files.
}
\examples{
ff <- tempfile()
cat(file = ff, "123456", "987654", sep = "\n")
read.fwf(ff, widths = c(1,2,3))    #> 1 23 456 \\ 9 87 654
read.fwf(ff, widths = c(1,-2,3))   #> 1 456 \\ 9 654
unlink(ff)
cat(file = ff, "123", "987654", sep = "\n")
read.fwf(ff, widths = c(1,0, 2,3))    #> 1 NA 23 NA \\ 9 NA 87 654
unlink(ff)
cat(file = ff, "123456", "987654", sep = "\n")
read.fwf(ff, widths = list(c(1,0, 2,3), c(2,2,2))) #> 1 NA 23 456 98 76 54
unlink(ff)
}
\keyword{file}
\keyword{connection}
