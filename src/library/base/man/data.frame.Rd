\name{data.frame}
\title{Data Frames}
\usage{
data.frame(\dots, row.names = NULL, check.rows = FALSE,
        check.names = TRUE)

as.data.frame(x)
is.data.frame(x)

row.names(data.frame.obj)
print(data.frame.obj)
}
\alias{data.frame}
\alias{as.data.frame}
\alias{is.data.frame}
\alias{row.names}
\alias{print.data.frame}
\arguments{
  \item{\dots}{these arguments are of either the form \code{value} or
    \code{tag=value}.  Component names are created based on the tag (if
    present) or the deparsed argument itself.}
  \item{row.names}{a character vector giving the row names for the data
    frame.}
  \item{check.rows}{if \code{TRUE} then the rows are checked for
    consistency of length and names.}
  \item{check.names}{if \code{TRUE} then the names of the variables
    in the data frame are checked to ensure that they are valid
    variable names.  If necessary they are adjusted so that they are.}
}
\value{
  For \code{data.frame(.)} a data frame.  Data frames are the
  fundamental data structure used by
  most of \R's modeling software.  They are tightly coupled collections
  of variables which share many of the properties of matrices.  The main
  difference being that the columns of a data frame may be of differing
  types (numeric, factor and character).

  \code{as.data.frame} attempts to coerce its argument to be a data
  frame.

  \code{is.data.frame} returns \code{TRUE} if its argument is a data
  frame and \code{FALSE} otherwise.
}
\seealso{
  \code{\link{read.table}}.
}
\examples{
str(d <- data.frame(cbind(x=1, y=1:10), ch=sample(LETTERS[1:3], 10, repl=TRUE)))
str(     data.frame(cbind(  1,   1:10),    sample(LETTERS[1:3], 10, repl=TRUE)))
is.data.frame(d)
all(1:10 == row.names(d))# TRUE (coercion)
}
\keyword{classes}
\keyword{methods}
