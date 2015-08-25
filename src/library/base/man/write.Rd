% File src/library/base/man/write.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{write}
\title{Write Data to a File}
\usage{
write(x, file = "data",
      ncolumns = if(is.character(x)) 1 else 5,
      append = FALSE, sep = " ")
}
\alias{write}
\arguments{
  \item{x}{the data to be written out, usually an atomic vector.}
  \item{file}{A connection, or a character string naming the file to write to.
    If \code{""}, print to the standard output connection.
#ifdef unix
    If it is \code{"|cmd"}, the output is piped to the command given
    by \file{cmd}.
#endif
  }
  \item{ncolumns}{the number of columns to write the data in.}
  \item{append}{if \code{TRUE} the data \code{x} are appended to the
    connection.}
  \item{sep}{a string used to separate columns.  Using \code{sep = "\t"}
    gives tab delimited output; default is \code{" "}.}
}
\description{
  The data (usually a matrix) \code{x} are written to file \code{file}.
  If \code{x} is a two-dimensional matrix you need to transpose it to get the
  columns in \code{file} the same as those in the internal representation.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{write} is a wrapper for \code{\link{cat}}, which gives further
  details on the format used.

  \code{\link{save}} for writing any \R objects,
  \code{\link{write.table}} for data frames,
  and \code{\link{scan}} for reading data.
}
\examples{
# create a 2 by 5 matrix
x <- matrix(1:10, ncol = 5)

# the file data contains x, two rows, five cols
# 1 3 5 7 9 will form the first row
write(t(x))

# Writing to the "console" 'tab-delimited'
# two rows, five cols but the first row is 1 2 3 4 5
write(x, "", sep = "\t")
unlink("data") # tidy up
}
\keyword{file}
\keyword{connection}
