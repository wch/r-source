\name{cbind}
\alias{cbind}%%--do C-internal method dispatching !!
\alias{rbind}
\alias{cbind.data.frame}
\alias{rbind.data.frame}
\concept{insert}
\title{Combine R Objects by Rows or Columns}
\description{
  Take a sequence of vector, matrix or data frames arguments and combine
  by \emph{c}olumns or \emph{r}ows, respectively.  These are generic
  functions with methods for other \R classes.
}
\usage{
cbind(\dots, deparse.level = 1)
rbind(\dots, deparse.level = 1)
}
\arguments{
  \item{\dots}{vectors or matrices.  These can be given as named arguments.}
  \item{deparse.level}{integer controlling the construction of labels;
    currently, \code{1} is the only possible value.}
}
\details{
  The functions \code{cbind} and \code{rbind} are generic, with methods
  for data frames.  The data frame method will be used if an argument is
  a data frame and the rest are vectors or matrices.  There can be other
  methods; in particular, there is one for time series objects.

  In the matrix case, all the vectors/matrices must be atomic (see
  \code{\link{vector}}) or lists (e.g., not expressions). 

  The \code{rbind} data frame method takes the classes of the columns
  from the first
  data frame.  Factors have their levels expanded as necessary (in
  the order of the levels of the levelsets of the factors encountered)
  and the result is an ordered factor if and only if all the components
  were ordered factors.  (The last point differs from S-PLUS.)

  If there are several matrix arguments, they must all have the same
  number of columns (or rows) and this will be the number of columns (or
  rows) of the result.  If all the arguments are vectors, the number of
  columns (rows) in the result is equal to the length of the longest
  vector.  Values in shorter arguments are recycled to achieve this
  length (with a \code{\link{warning}} if they are recycled only
  \emph{fractionally}).

  When the arguments consist of a mix of matrices and vectors the number
  of columns (rows) of the result is determined by the number of columns
  (rows) of the matrix arguments.  Any vectors have their values
  recycled or subsetted to achieve this length.

  For \code{cbind} (\code{rbind}), vectors of zero length (including
  \code{NULL}) are ignored unless the result would have zero rows
  (columns),  for S compatibility.
  (Zero-extent matrices do not occur in S3 and are not ignored in \R.)
}
\value{
  A matrix or data frame combining the \code{\dots} arguments
  column-wise or row-wise.

  For \code{cbind} (\code{rbind}) the column (row) names are taken from
  the names of the arguments, or where those are not supplied by
  deparsing the expressions given (if that gives a sensible name).
  The names will depend on whether data frames are included:
  see the examples.
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
   necessary to coerce them to data frames first.  (Note that this
   algorithm can result in calling the data frame method if the
   arguments are all either data frames or vectors, and this will result in
   the coercion of character vectors to factors.)
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{c}} to combine vectors (and lists) as vectors,
  \code{\link{data.frame}} to combine vectors and matrices as a data
  frame.
}
\examples{
m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
m
m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column 
m
cbind(1:7, diag(3))# vector is subset -> warning

cbind(0, rbind(1, 1:3))
cbind(I=0, X=rbind(a=1, b=1:3))  # use some names
xx <- data.frame(I=rep(0,2))
cbind(xx, X=rbind(a=1, b=1:3))   # named differently

cbind(0, matrix(1, nrow=0, ncol=4))#> Warning (making sense)
dim(cbind(0, matrix(1, nrow=2, ncol=0)))#-> 2 x 1
}
\keyword{array}
\keyword{manip}
