\name{cbind}
\alias{cbind}%%--do C-internal method dispatching !!
\alias{rbind}
\alias{cbind.data.frame}
\alias{rbind.data.frame}
\title{Combine Columns/Rows into a Matrix}
\description{
  Take a sequence of vector and/or matrix arguments and combine them as
  the \emph{c}olumns or \emph{r}ows, respectively, of a matrix.
}
\usage{
cbind(\dots)
rbind(\dots)
}
\details{
  The functions \code{cbind} and \code{rbind} are generic, with methods
  for data frames.

  If there are several matrix arguments, they must all have the same
  number of columns (or rows) and this will be the number of columns (or
  rows) of the result.  If all the arguments are vectors, the number of
  columns (rows) in the result is equal to the length of the longest
  vector.  Values in shorter arguments are recycled to achieve this
  length (with a \code{\link{warning}} when they are recycled only
  \emph{fractionally}).

  When the arguments consist of a mix of matrices and vectors the number
  of columns (rosw) of the result is determined by the number of columns
  (rows) of the matrix arguments.  Any vectors have their values
  recycled or subsetted to achieve this length.
}
\note{
  The method dispatching is \emph{not} done via
  \code{\link{UseMethod}(..)}, but by C-internal dispatching.
  Therefore, there's no need for, e.g., \code{rbind.default}.
}
\seealso{
  \code{\link{c}} to combine vectors (and lists).
}
\examples{
cbind(1,1:7) # the '1' (= shorter vector) is recycled
cbind(1:7, diag(3))# vector is subset -> warning

cbind(0,rbind(1,1:3))

cbind(0, matrix(1, nrow=0, ncol=4))#> Warning (making sense)
dim(cbind(0, matrix(1, nrow=2, ncol=0)))#-> 2 x 1
}
\keyword{array}
\keyword{manip}
