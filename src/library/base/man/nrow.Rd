\name{nrow, ncol}
\title{The Number of Rows/Columns of an Array}
\usage{
nrow(x)
ncol(x)
NCOL(x)
NROW(x)
}
\alias{nrow}
\alias{NROW}
\alias{ncol}
\alias{NCOL}
\arguments{
  \item{x}{a vector, array or data frame}
}
\description{
  \code{nrow} and \code{ncol} return the number of rows or columns
  present in \code{x}. 
  \code{NCOL} and \code{NROW} do the same, but treat a vector as
  1-column matrix.
}
\seealso{
  \code{\link{dim}} which returns \emph{all} dimensions;
  \code{\link{array}}, \code{\link{matrix}}.
}
\examples{
ma <- matrix(1:12, 3, 4)
nrow(ma)   # 3
ncol(ma)   # 4

ncol(array(1:24, dim = 2:4)) # 3, the second dimension
NCOL(1:12) # 1
NROW(1:12) # 12
}
\keyword{array}
