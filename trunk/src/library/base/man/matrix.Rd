\name{matrix}
\alias{matrix}
\alias{as.matrix}
\alias{as.matrix.default}
\alias{as.matrix.data.frame}
\alias{is.matrix}
\title{Matrices}
\description{
  \code{matrix} creates a matrix from the given set of values.

  \code{as.matrix} attempts to turn its argument into a matrix.

  \code{is.matrix} tests if its argument is a (strict) matrix.
}
\usage{
matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
       dimnames = NULL)

as.matrix(x, \dots)
\method{as.matrix}{data.frame}(x, rownames.force = NA, \dots)

is.matrix(x)
}
\arguments{
  \item{data}{an optional data vector.}
  \item{nrow}{the desired number of rows.}
  \item{ncol}{the desired number of columns.}
  \item{byrow}{logical. If \code{FALSE} (the default) the matrix is
    filled by columns, otherwise the matrix is filled by rows.}
  \item{dimnames}{A \code{\link{dimnames}} attribute for the matrix: a
    \code{list} of length 2 giving the row and column names respectively.}
  \item{x}{an \R object.}
  \item{\dots}{additional arguments to be passed to or from methods.}
  \item{rownames.force}{logical indicating if the resulting matrix
    should have character (rather than \code{NULL})
    \code{\link{rownames}}.  The default, \code{NA}, uses \code{NULL}
    rownames if the data frame has \sQuote{automatic} row.names or for a
    zero-row data frame.}
}
\details{
  If either of \code{nrow} or \code{ncol} is not given, an attempt is
  made to infer it from the length of \code{data} and the other
  parameter.

  If there are too few elements in \code{data} to fill the array,
  then the elements in \code{data} are recycled.  If \code{data} has
  length zero, \code{NA} of an appropriate type is used for atomic
  vectors (\code{0} for raw vectors) and \code{NULL} for lists.

  \code{is.matrix} returns \code{TRUE} if \code{x} is a matrix and has a
  \code{\link{dim}} attribute of length 2) and \code{FALSE} otherwise.
  Note that a \code{\link{data.frame}} is \strong{not} a matrix by this
  test.  It is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.

  \code{as.matrix} is a generic function.  The method for data frames
  will return a character matrix if there is any
  non-(numeric/logical/complex) column, applying \code{\link{format}} to
  non-character columns.  Otherwise, the usual coercion hierarchy
  (logical < integer < double < complex) will be used, e.g., all-logical
  data frames will be coerced to a logical matrix, mixed logical-integer
  will give a integer matrix, etc.

  When coercing a vector, it produces a one-column matrix, and
  promotes the names (if any) of the vector to the rownames of the matrix.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{data.matrix}}, which attempts to convert to a numeric matrix.
}
\examples{
is.matrix(as.matrix(1:10))
!is.matrix(warpbreaks)# data.frame, NOT matrix!
warpbreaks[1:10,]
as.matrix(warpbreaks[1:10,]) #using as.matrix.data.frame(.) method

# Example of setting row and column names
mdat <- matrix(c(1,2,3, 11,12,13), nrow = 2, ncol=3, byrow=TRUE,
               dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3")))
mdat
}
\keyword{array}
\keyword{algebra}
