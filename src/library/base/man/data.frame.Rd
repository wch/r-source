\name{data.frame}
\title{Data Frames}
\usage{
data.frame(\dots, row.names = NULL, check.rows = FALSE,
        check.names = TRUE)

as.data.frame(x)
is.data.frame(x)

row.names(data.frame.obj)
row.names(data.frame.obj) <- names
print(data.frame.obj, \dots, digits = NULL, quote = FALSE, right = TRUE)
plot (data.frame.obj, \dots)
}
\alias{data.frame}
\alias{as.data.frame}
\alias{is.data.frame}
\alias{row.names}
\alias{row.names<-}
%- methods:
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
\alias{xpdrows.data.frame}
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
    variable names.  If necessary they are adjusted (by
    \code{\link{make.names}}) so that they are.}
  \item{data.frame.obj}{objects of class \code{data.frame}.}
  \item{\dots}{optional arguments to \code{print} or \code{plot} methods.}
}
\description{
  These functions create or manipulate data frames, tightly coupled collections
  of variables which share many of the properties of matrices and of
  lists, used as the fundamental data structure by most of \R's
  modeling software.
}
\value{
  For \code{data.frame(.)} a data frame, a matrix-like stucture whose
  columns may be of differing types (numeric, factor and character).

  \code{as.data.frame} is generic function with many \code{\link{methods}}.
  It attempts to coerce its argument to be a data frame.

  \code{is.data.frame} returns \code{TRUE} if its argument is a data
  frame and \code{FALSE} otherwise.

  \code{row.names} can be used to set and retrieve the row names of a
  data frame, similarly to \code{\link{rownames}} for arrays.

  \code{plot.data.frame}, a method of the \code{\link{plot}} generic,
  uses \code{\link{stripplot}} for \emph{one} variable,
  \code{\link{plot.default}} (scatterplot) for \emph{two} variables, and
  \code{\link{pairs}} (scatterplot matrix) otherwise.

  For the \code{\link{print}} method (\code{print.data.frame}), see
  \code{\link{print.matrix}}.

  \code{xpdrows.data.frame} is an auxiliary function which expands the
  rows of a data frame.  It is used by the data frame methods of
  \code{[<-} and \code{[[<-} (which perform subscripted assignments on a
  data frame), and not intended to be called directly.
}
\details{
  Non-numeric variables passed to \code{data.frame} are converted to
  factor columns unless protected by \code{\link{I}}. This applies to
  character and logical variables, in particular. It also applies to
  adding columns to a data frame.

  If a list or data frame or matrix is passed to \code{data.frame}
  it is as if each column had been passed as a separate argument,
  with the exception of matrices of class \code{\link{model.matrix}}.
}
\seealso{
  \code{\link{read.table}}, \code{\link{Math.data.frame}},etc, about
  \emph{Group} methods for \code{data.frame}s;  \code{\link{make.names}}.
}
\examples{
L3 <- LETTERS[1:3]
str(d <- data.frame(cbind(x=1, y=1:10), ch=sample(L3, 10, repl=TRUE)))
str(     data.frame(cbind(  1,   1:10),    sample(L3, 10, repl=TRUE)))
is.data.frame(d)
all(1:10 == row.names(d))# TRUE (coercion)
}
\keyword{classes}
\keyword{methods}
