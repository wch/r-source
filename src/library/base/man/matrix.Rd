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
  It is generic: you can write methods to handle
  specific classes of objects, see \link{InternalMethods}.
}
\usage{
matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
as.matrix(x)
is.matrix(x)
}
\arguments{
  \item{data}{an optional data vector.}
  \item{nrow}{the desired number of rows}
  \item{ncol}{the desired number of columns}
  \item{byrow}{logical. If \code{FALSE}
    (the default) the matrix is filled by columns, otherwise the matrix is
    filled by rows.}
  \item{dimnames}{A \code{\link{dimnames}} attribute for the matrix: a
    \code{list} of length 2 giving the row and column names respectively.}
  \item{x}{an \R object.}
}
\details{
  If either of \code{nrow} or \code{ncol} is not given, an attempt is
  made to infer it from the length of \code{data} and the other
  parameter.

  If there are too few elements in \code{data} to fill the array,
  then the elements in \code{data} are recycled.  If \code{data} has
  length zero, \code{NA} of an appropriate type is used for atomic
  vectors (\code{0} for raw vectors) and \code{NULL} for lists.

  \code{is.matrix} returns \code{TRUE} if \code{x} is a matrix (i.e., it
  is \emph{not} a \code{\link{data.frame}} and has a \code{\link{dim}}
  attribute of length 2) and \code{FALSE} otherwise.

  \code{as.matrix} is a generic function. The method for data frames
  will convert any non-numeric/complex column into a character
  vector using \code{\link{format}} and so return a character matrix,
  except that all-logical data frames will be coerced to a logical matrix.
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
