\name{matrix}
\title{Create a Matrix}
\usage{
matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
as.matrix(x)
is.matrix(x)
}
\alias{matrix}
\alias{as.matrix}
\alias{is.matrix}
\description{
  \code{matrix} creates a matrix from the given set of values.
  \code{nrow} gives the desired number of rows and \code{ncol} the
  desired number of columns.  If either of \code{nrow} or \code{ncol} is
  not given, an attempt is made to infer it from the length of
  \code{data} and the other parameter.  If \code{byrow} is \code{FALSE}
  (the default) the matrix is filled by columns, otherwise the matrix is
  filled by rows.  A \code{dimnames} attribute for the matrix can be
  specified with the optional \code{dimnames} argument (which thus has
  to be a list of length 2).

  \code{as.matrix} attempts to turn its argument into a matrix.  This
  function is generic.

  \code{is.matrix} returns \code{TRUE} if \code{x} is a matrix (i.e., it
  has a \code{dim} attribute of length 2) and \code{FALSE} otherwise.
}
\keyword{array}
\keyword{algebra}
