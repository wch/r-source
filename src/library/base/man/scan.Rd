\name{scan}
\title{Read Data Values}
\usage{
scan(file="", what=numeric(), nmax=0, sep="",
        skip=0, nlines=0)
}
\alias{scan}
\arguments{
\item{file}{the name of a file to read data values from.
If the specified file is \code{""},
then input is taken from the keyboard
(in this case input can be terminated by a blank line).}
\item{what}{the type of \code{what} gives the type of data to be read.
If \code{what} is a list it is assumed that the lines of the data
file are records which each contain \code{length(what)} items.}
\item{nmax}{the maximum number of data values to be read, or if
\code{what} is a list, the maximum number of records to be read.}
\item{sep}{by default, scan expects to read white-space delimited
input fields.  Alternatively, \code{sep} can be used to specify a
character which delimits fields.}
\item{skip}{this many lines of the input file should be skipped
before starting to read data values.}
\item{nlines}{the maximum number of lines of data to be read.}
}
\description{
The value of \code{what} can be a list of types, in which case
\code{scan} returns a list of vectors with the types given by the
types of the elements in \code{what}.  This provides a way of reading
columnar data.

Keyboard entry is terminated by typing a blank line.
}
\seealso{
\code{\link{read.table}} for more user-friendly reading of data matrices.
\code{\link{write}}.
}
\examples{
system("echo 'TITEL extra line' > ex.data")
system("echo 2 3 5 7 >> ex.data")
system("echo 11 13 17 >> ex.data")
scan("ex.data", skip = 1)
}
\keyword{file}
