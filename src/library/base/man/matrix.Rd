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
    \code{list} of length 2.}
  \item{x}{an \R object.}
}
\details{
  If either of \code{nrow} or \code{ncol} is not given, an attempt is
  made to infer it from the length of \code{data} and the other
  parameter.

  \code{is.matrix} returns \code{TRUE} if \code{x} is a matrix (i.e., it
  is \emph{not} a \code{\link{data.frame}} and has a \code{\link{dim}}
  attribute of length 2) and \code{FALSE} otherwise.
}
\seealso{\code{\link{data.matrix}}.}
\examples{
is.matrix(as.matrix(1:10))
data(warpbreaks)
!is.matrix(warpbreaks)# data.frame, NOT matrix!
str(warpbreaks)
str(as.matrix(warpbreaks))#using as.matrix.data.frame(.) method
}
\keyword{array}
\keyword{algebra}
