\name{read.table}
\alias{read.table}
\alias{read.csv}
\alias{read.csv2}
\title{Data Input}
\description{
  Reads a file in table format and creates a data frame from it, with
  cases corresponding to lines and variables to fields in the file.
}
\usage{
read.table(file, header = FALSE, sep = "", quote = "\"'", dec = ".",
           row.names, col.names, as.is = FALSE, na.strings = "NA",
           skip = 0, check.names = TRUE, strip.white = FALSE)

read.csv(file, header = TRUE, sep = ",", quote="\"", dec=".", \dots)

read.csv2(file, header = TRUE, sep = ";", quote="\"", dec=",", \dots)
}
\details{
  \code{read.csv} and \code{read.csv2} are identical to
  \code{read.table} except for the defaults. They are intended for
  reading "comma separated variable" files (.csv) or the variant used in
  countries that use comma as decimal point and consequently semicolon
  as field separator.  Notice that \code{header=TRUE} in these variants.
}
\arguments{
  \item{file}{the name of the file which the data are to be read from.
    Each row of the table appears as one line of the file.  If it does
    not contain an \emph{absolute} path, the file name is
    \emph{relative} to the current working directory,
    \code{\link{getwd}()}.}

  \item{header}{a logical value indicating whether the file contains the
    names of the variables as its first line.}

  \item{sep}{the field separator character.  Values on each line of the
    file are separated by this character.}
  
  \item{quote}{the set of quoting characters. To disable quoting
    altogether, use \code{quote=""}.}

  \item{dec}{the character used in the fike for decimal points.}

  \item{row.names}{a vector of row names.  This can be a vector giving
    the actual row names, or a single number giving the column of the
    table which contains the row names, or character string giving the
    name of the table column containing the row names.}

  \item{col.names}{a vector of optional names for the variables.
    The default is to use \code{"V"} followed by the column number.}

  \item{as.is}{the default behavior of \code{read.table} is to convert
    non-numeric variables to factors.  The variable \code{as.is}
    controls this conversion.  Its value is either a vector of logicals
    (values are recycled if necessary), or a vector of numeric indices
    which specify which columns should be left as character strings.}

  \item{na.strings}{a vector strings which are to be interpreted as
    \code{\link{NA}} values.}

  \item{skip}{the number of lines of the data file to skip before
    beginning to read data.}

  \item{check.names}{logical. If \code{TRUE} then the names of the
    variables in the data frame are checked to ensure that they are
    valid variable names.  If necessary they are adjusted (by
    \code{\link{make.names}}) so that they are.}

  \item{skip.white}{logical. Used only when \code{sep} has
    been specified, and allows the stripping of leading and trailing
    white space from \code{character} fields (\code{numeric} fields
    are always stripped).  See \code{\link{scan}} for further details,
    remembering that the columns may include the row names.}

  \item{\dots}{Further arguments to \code{read.table}.}
}
\value{
  A data frame (\code{\link{data.frame}}) containing a representation of
  the data in the file.

  This function is the principal means of reading tabular data into \R.
}
\note{
  The implementation of \code{read.table} currently reads everything as
  character using \code{\link{scan}} and subsequently defines
  \code{"numeric"} or \code{\link{factor}} variables.

  This is quite memory consuming for files of thousands of records and
  may need larger memory, see \code{\link{Memory}}.
}
\seealso{
  \code{\link{scan}},
  \code{\link{read.fwf}} for reading \emph{f}ixed \emph{w}idth
  \emph{f}ormatted input;
  \code{\link{write.table}};
  \code{\link{data.frame}}. 
}
\keyword{file}
