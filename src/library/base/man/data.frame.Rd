\name{data.frame}
\title{Data Frames}
\usage{
data.frame(\dots, row.names = NULL, check.rows = FALSE,
        check.names = TRUE)

as.data.frame(x)
is.data.frame(x)

row.names(data.frame.obj)
row.names(data.frame.obj) <- names
print(data.frame.obj)
plot(data.frame.obj)
}
\alias{data.frame}
\alias{as.data.frame}
\alias{is.data.frame}
\alias{row.names}
\alias{row.names<-}
%- methods:
\alias{row.names.default}
\alias{row.names<-.data.frame}
\alias{row.names<-.default}
\alias{print.data.frame}
\alias{plot.data.frame}
%
\alias{as.data.frame.AsIs}
\alias{as.data.frame.character}
\alias{as.data.frame.complex}
\alias{as.data.frame.data.frame}
\alias{as.data.frame.default}
\alias{as.data.frame.factor}
\alias{as.data.frame.integer}
\alias{as.data.frame.list}
\alias{as.data.frame.logical}
\alias{as.data.frame.matrix}
\alias{as.data.frame.model.matrix}
\alias{as.data.frame.numeric}
\alias{as.data.frame.ordered}
\alias{as.data.frame.ts}
\alias{as.data.frame.vector}
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

  \code{as.data.frame} is generic function with many \code{\link{methods}}.
  It attempts to coerce its argument to be a data frame.

  \code{is.data.frame} returns \code{TRUE} if its argument is a data
  frame and \code{FALSE} otherwise.

  \code{row.names} is generic and is used to set and retrieve the row
  names of a data frame, similarly to \code{\link{rownames}} for arrays.
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
