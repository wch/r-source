\name{cbind}
\alias{cbind}%%--do C-internal method dispatching !!
\alias{rbind}
\alias{cbind.data.frame}
\alias{rbind.data.frame}
\title{Combine R Objects by Rows or Columns}
\description{
  Take a sequence of vector, matrix or data frames arguments and combine
  by \emph{c}olumns or \emph{r}ows, respectively.  There may be methods
  for other \R classes.
}
\usage{
cbind(\dots, deparse.level = 1)
rbind(\dots, deparse.level = 1)
}
\arguments{
  \item{\dots}{vectors or matrices.}
  \item{deparse.level}{integer controlling the construction of labels;
    currently, \code{1} is the only possible value.}
}
\details{
  The functions \code{cbind} and \code{rbind} are generic, with methods
  for data frames.  The data frame method will be used if an argument is
  a data frame and the rest are vectors or matrices.  There can be other
  methods, for example \code{\link[ts:ts.union]{cbind.ts}} in package
  \code{ts}.

  If there are several matrix arguments, they must all have the same
  number of columns (or rows) and this will be the number of columns (or
  rows) of the result.  If all the arguments are vectors, the number of
  columns (rows) in the result is equal to the length of the longest
  vector.  Values in shorter arguments are recycled to achieve this
  length (with a \code{\link{warning}} when they are recycled only
  \emph{fractionally}).

  When the arguments consist of a mix of matrices and vectors the number
  of columns (rows) of the result is determined by the number of columns
  (rows) of the matrix arguments.  Any vectors have their values
  recycled or subsetted to achieve this length.
}
\note{
  The method dispatching is \emph{not} done via
  \code{\link{UseMethod}()}, but by C-internal dispatching.
  Therefore, there is no need for, e.g., \code{rbind.default}.

  The dispatch algorithm is described in the source file
  (\file{.../src/main/bind.c}) as

  \enumerate{
    \item For each argument we get the list of possible class
    memberships from the class attribute.

    \item We inspect each class in turn to see if there is an
    an applicable method.

    \item If we find an applicable method we make sure that it is
     identical to any method determined for prior arguments.
     If it is identical, we proceed, otherwise we immediately
     drop through to the default code.
   }
   If you want to combine other objects with data frames, it may be
   necessary to coerce them to data frames first.
}
\seealso{
  \code{\link{c}} to combine vectors (and lists) as vectors,
  \code{\link{data.frame}} to combine vectors and matrices as a data
  frame.
}
\examples{
cbind(1, 1:7) # the `1' (= shorter vector) is recycled
cbind(1:7, diag(3))# vector is subset -> warning

cbind(0, rbind(1, 1:3))

cbind(0, matrix(1, nrow=0, ncol=4))#> Warning (making sense)
dim(cbind(0, matrix(1, nrow=2, ncol=0)))#-> 2 x 1
}
\keyword{array}
\keyword{manip}
