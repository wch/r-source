\name{data.frame}
\title{Data Frames}
\alias{data.frame}
\alias{as.data.frame}
\alias{is.data.frame}
\alias{row.names}
\alias{row.names.data.frame}
\alias{row.names.default}
\alias{row.names<-}
\alias{row.names<-.data.frame}
\alias{row.names<-.default}
%- methods:
\alias{print.data.frame}
\alias{plot.data.frame}
%
\alias{[.AsIs}
\alias{as.data.frame.AsIs}
% FIXME: I(.) is not documented enough; "AsIs" does not appear anywhere
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
\description{
  These functions create or manipulate data frames, tightly coupled
  collections of variables which share many of the properties of
  matrices and of lists, used as the fundamental data structure by most
  of \R's modeling software.
}
\usage{
data.frame(\dots, row.names = NULL, check.rows = FALSE,
        check.names = TRUE)

as.data.frame(x, row.names = NULL, optional)
is.data.frame(x)

row.names(x)
row.names(x) <- names
print(x, \dots, digits = NULL, quote = FALSE, right = TRUE)
plot (x, \dots)
}
\arguments{
  \item{\dots}{these arguments are of either the form \code{value} or
    \code{tag=value}.  Component names are created based on the tag (if
    present) or the deparsed argument itself.}
  \item{row.names}{a character vector giving the row names for the data
    frame.}
  \item{check.rows}{if \code{TRUE} then the rows are checked for
    consistency of length and names.}
  \item{check.names}{logical.  If \code{TRUE} then the names of the
    variables in the data frame are checked to ensure that they are
    syntactically valid variable names.  If necessary they are adjusted
    (by \code{\link{make.names}}) so that they are.}
  \item{data.frame.obj}{objects of class \code{data.frame}.}
  \item{\dots}{optional arguments to \code{print} or \code{plot} methods.}
  \item{optional}{logical. If true, setting row names is optional.}
}
\value{
  For \code{data.frame(.)} a data frame, a matrix-like stucture whose
  columns may be of differing types (numeric, factor and character).

  \code{as.data.frame} is generic function with many methods.   It
  attempts to coerce its argument to be a data frame.

  \code{is.data.frame} returns \code{TRUE} if its argument is a data
  frame and \code{FALSE} otherwise.

  \code{row.names} can be used to set and retrieve the row names of a
  data frame, similarly to \code{\link{rownames}} for arrays (and
  it is a generic function that calls \code{rownames} for an
  array argument.} 

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
  factor columns unless protected by \code{\link{I}}.  This applies to
  character and logical variables, in particular.  It also applies to
  adding columns to a data frame.

  If a list or data frame or matrix is passed to \code{data.frame}
  it is as if each column had been passed as a separate argument,
  with the exception of matrices of class \code{\link{model.matrix}}.
}
\seealso{
  \code{\link{read.table}},
  \code{\link{Math.data.frame}} etc, about
  \emph{Group} methods for \code{data.frame}s;
  \code{\link{make.names}}.
}
\examples{
L3 <- LETTERS[1:3]
str(d <- data.frame(cbind(x=1, y=1:10), fac=sample(L3, 10, repl=TRUE)))

## The same with automatic column names:
str(     data.frame(cbind(  1,   1:10),     sample(L3, 10, repl=TRUE)))
is.data.frame(d)

## do not convert to factor, using I() :
str(cbind(d, char = I(letters[1:10])), vec.len = 10)

all(1:10 == row.names(d))# TRUE (coercion)
}
\keyword{classes}
\keyword{methods}
