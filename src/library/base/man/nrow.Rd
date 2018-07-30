% File src/library/base/man/nrow.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{nrow}
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
  \item{x}{a vector, array, data frame, or \code{\link{NULL}}.}
}
\description{
  \code{nrow} and \code{ncol} return the number of rows or columns
  present in \code{x}.
  \code{NCOL} and \code{NROW} do the same treating a vector as
  1-column matrix, even a 0-length vector, compatibly with
  \code{\link{as.matrix}()} or \code{\link{cbind}()}, see the example.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole (\code{ncol} and \code{nrow}.)
}
\seealso{
  \code{\link{dim}} which returns \emph{all} dimensions;
  \code{\link{array}}, \code{\link{matrix}}.
}
\value{an \code{\link{integer}} of length 1 or \code{\link{NULL}}, the
  latter only for \code{ncol} and \code{nrow}.}
\examples{
ma <- matrix(1:12, 3, 4)
nrow(ma)   # 3
ncol(ma)   # 4

ncol(array(1:24, dim = 2:4)) # 3, the second dimension
NCOL(1:12) # 1
NROW(1:12) # 12

## as.matrix() produces 1-column matrices from 0-length vectors,
## and so does cbind() :
dim(as.matrix(numeric())) # 0 1
dim(    cbind(numeric())) # ditto
## consequently, NCOL(.) gives 1, too :
NCOL(numeric()) # 1 and hence
NCOL(NULL)      # 1
}
\keyword{array}
