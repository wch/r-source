% File src/library/utils/man/count.fields.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{count.fields}
\title{Count the Number of Fields per Line}
\usage{
count.fields(file, sep = "", quote = "\"'", skip = 0,
             blank.lines.skip = TRUE, comment.char = "#")
}
\alias{count.fields}
\arguments{
  \item{file}{a character string naming an ASCII data file, or a
    \code{\link{connection}}, which will be opened if necessary,
    and if so closed at the end of the function call.}

  \item{sep}{the field separator character.  Values on each line of the
    file are separated by this character.  By default, arbitrary amounts
    of whitespace can separate fields.}

  \item{quote}{the set of quoting characters}

  \item{skip}{the number of lines of the data file to skip before
    beginning to read data.}

  \item{blank.lines.skip}{logical: if \code{TRUE} blank lines in the
    input are ignored.}

  \item{comment.char}{character: a character vector of length one
    containing a single character or an empty string.}
}
\description{
  \code{count.fields} counts the number of fields, as separated by
  \code{sep}, in each of the lines of \code{file} read.
}
\details{
  This used to be used by \code{\link{read.table}} and can still be
  useful in discovering problems in reading a file by that function.

  For the handling of comments, see \code{\link{scan}}.
  
  Consistent with \code{\link{scan}}, \code{count.fields} allows
  quoted strings to contain newline characters.  In such a case the
  starting line will have the field count recorded as \code{NA}, and
  the ending line will include the count of all fields from the
  beginning of the record.
  
}
\value{
  A vector with the numbers of fields found.
}
\seealso{
  \code{\link{read.table}}
}
\examples{
cat("NAME", "1:John", "2:Paul", file = "foo", sep = "\n")
count.fields("foo", sep = ":")
unlink("foo")
}
\keyword{file}
