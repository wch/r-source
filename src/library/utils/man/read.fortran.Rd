\name{read.fortran}
\alias{read.fortran}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Read fixed-format data }
\description{
Read fixed-format data files using Fortran-style format specifications.
}
\usage{
read.fortran(file, format, ..., as.is = TRUE, colClasses = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{File or connection to read from}
  \item{format}{Character vector or list of vectors. See Details below.}
  \item{\dots}{Other arguments for \code{read.table}}
  \item{as.is}{Keep characters as characters?}
  \item{colClasses}{Variable classes to override defaults. See
    \code{\link{read.table}} for details.}
}
\details{
  The format for a field is of one of the following forms: \code{rFl.d},
  \code{rDl.d}, \code{rXl}, \code{rAl}, \code{rIl}, where \code{l} is
  the number of columns, \code{d} is the number of decimal places, and
  \code{r} is the number of repeats. \code{F} and \code{D} are numeric
  formats, \code{A} is character, \code{I} is integer, and \code{X}
  indicates columns to be skipped. The repeat code \code{r} and decimal
  place code \code{d} are always optional. The length code \code{l} is
  required except for \code{X} formats when \code{r} is present.
  
  For a single-line record, \code{format} should be a character
  vector. For a multiline record it should be a list with a character
  vector for each line.
  
  Skipped (\code{X}) columns are not passed to \code{read.table}, so
  \code{colClasses}, \code{col.names}, and similar arguments passed to
  \code{read.table} should not reference these columns.
}
\value{
  A data frame
}

\seealso{\code{\link{read.fwf}}, \code{\link{read.csv}}}
\examples{
ff <- tempfile()
cat(file=ff, "123456", "987654", sep="\n")
read.fortran(ff, c("F2.1","F2.0","I2"))
read.fortran(ff, c("2F1.0","2X","2A1"))
unlink(ff)
cat(file=ff, "123456AB", "987654CD", sep="\n")
read.fortran(ff, list(c("2F3.1","A2"), c("3I2","2X")))
unlink(ff)
}
\keyword{file}% at least one, from doc/KEYWORDS
\keyword{connection}% __ONLY ONE__ keyword per line
