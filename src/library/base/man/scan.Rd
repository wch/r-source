\name{scan}
\title{Read Data Values}
\usage{
scan(file = "", what = double(0), nmax = -1, n = -1, sep = "",
     quote = "", dec = ".", skip = 0, nlines = 0, na.strings = "NA",
     flush = FALSE, strip.white = FALSE, quiet = FALSE)
}
\alias{scan}
\arguments{
  \item{file}{the name of a file to read data values from.  If the
    specified file is \code{""}, then input is taken from the keyboard
    (in this case input can be terminated by a blank line).

    Otherwise, the file name is \emph{relative} to the current working
    directory, \code{\link{getwd}()}, unless it specifies an
    \emph{absolute} path.}
  \item{what}{the type of \code{what} gives the type of data to be
    read.  If \code{what} is a list, it is assumed that the lines of the
    data file are records each containing \code{length(what)} items
    (``fields'').}
  \item{nmax}{the maximum number of data values to be read, or if
    \code{what} is a list, the maximum number of records to be read.  If
    omitted, \code{scan} will read to the end of \code{file}.}
  \item{n}{the maximum number of data values to be read, defaulting to
    no limit.}
  \item{sep}{by default, scan expects to read white-space delimited
      input fields.  Alternatively, \code{sep} can be used to specify a
      character which delimits fields.} 
  \item{quote}{the set of quoting characters as a single character string.}
  \item{dec}{decimal point character.}
  \item{skip}{this many lines of the input file should be skipped before
    starting to read data values.}
  \item{nlines}{the maximum number of lines of data to be read.}
  \item{na.strings}{character string, indicating \emph{which} character
    fields in the file should translate to missing (\code{\link{NA}})
    values.}
  \item{flush}{logical; if \code{TRUE}, \code{scan} will flush to the
    end of the line after reading the last of the fields requested.
    This allows putting comments after the last field.}
  \item{strip.white}{vector of logical value(s) corresponding to items
    in the \code{what} argument. It is used only when \code{sep} has
    been specified, and allows to strip leading and trailing white space
    from \code{character} fields (\code{numeric} fields are always
    stripped).

    If \code{strip.white} is of length 1, it applies to all fields;
    otherwise, if \code{strip.white[i]} is \code{TRUE} \emph{and} the
    \code{i}-th field is character (because \code{what[i]} is), then the
    leading and trailing white space from field \code{i} is stripped.}
  \item{quiet}{logical; if \code{FALSE} (default), scan(.) will print a
    line, telling what fields have been read.}
}
\description{
  Read data into a vector or list from the console or file.
}
\details{
  The value of \code{what} can be a list of types, in which case
  \code{scan} returns a list of vectors with the types given by the
  types of the elements in \code{what}.  This provides a way of reading
  columnar data.

  If \code{sep} is nondefault, the fields may be quoted in the style of
  .csv format files where separators inside quotes (\code{''} or
  \code{""}) are ignored and quotes may be put inside strings by
  doubling them.
  
  Keyboard entry is terminated by typing a blank line.
}
\seealso{
  \code{\link{read.table}} for more user-friendly reading of data
  matrices;
  \code{\link{write}}.
}
\examples{
cat("TITLE extra line", "2 3 5 7", "11 13 17", file="ex.data", sep="\n")
pp <- scan("ex.data", skip = 1, quiet= TRUE)
    scan("ex.data", skip = 1)
    scan("ex.data", skip = 1, nlines=1)# only 1 line after the skipped one
str(scan("ex.data", what = list("","",""))) # flush is F -> read "7"
str(scan("ex.data", what = list("","",""), flush = TRUE))
unlink("ex.data") # tidy up
}
\keyword{file}
