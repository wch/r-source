\name{row/colnames}
\title{Row and Column Names}
\alias{rownames}
\alias{rownames<-}
\alias{colnames}
\alias{colnames<-}
\usage{
rownames(x, do.NULL = TRUE, prefix = "row")
rownames(x) <- value

colnames(x, do.NULL = TRUE, prefix = "col")
colnames(x) <- value
}
\description{
  Retrieve or set the row or column names of a matrix-like object.
}
\arguments{
  \item{x}{a matrix-like \R object, with at least two dimensions for
    \code{colnames}.}
  \item{do.NULL}{logical.  Should this create names if they are
    \code{NULL}?}
  \item{prefix}{for created names.}
  \item{value}{a valid value for that component of
    \code{\link{dimnames}(x)}.  For a matrix or array this is either
    \code{NULL} or a character vector of length the appropriate
    dimension.}
}
\details{
  The extractor functions try to do something sensible for any
  matrix-like object \code{x}.  If the object has \code{\link{dimnames}}
  the first component is used as the row names, and the second component
  (if any) is used for the col names.  For a data frame, \code{rownames}
  and \code{colnames} are equivalent to \code{\link{row.names}} and
  \code{\link{names}} respectively.

  If \code{do.NULL} is \code{FALSE}, a character vector (of length
  \code{\link{NROW}(x)} or \code{\link{NCOL}(x)}) is returned in any
  case, prepending \code{prefix} to simple numbers, if there are no
  dimnames or the corresponding component of the dimnames is \code{NULL}.

  For a data frame, \code{value} for \code{rownames} should be a
  character vector of unique names, and for \code{colnames} a character
  vector of unique syntactically-valid names.  (Note: uniqueness and
  validity are not enforced.)
}
\seealso{
  \code{\link{dimnames}},
  \code{\link{case.names}},
  \code{\link{variable.names}}.
}
\examples{
m0 <- matrix(NA, 4, 0)
rownames(m0)

m2 <- cbind(1,1:4)
colnames(m2, do.NULL = FALSE)
colnames(m2) <- c("x","Y")
rownames(m2) <- rownames(m2, do.NULL = FALSE, prefix = "Obs.")
m2
}
\keyword{array}
\keyword{manip}
