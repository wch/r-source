\name{col}
\title{Column Indexes}
\usage{
col(x, as.factor=FALSE)
}
\alias{col}
\arguments{
\item{x}{a matrix.}
\item{as.factor}{a logical value indicating whether the
value should be returned as a factor rather than as numeric.}
}
\value{
An integer matrix with the same dimensions as \code{x} and whose
\code{ij}-th element is equal to \code{j}.
}
\seealso{
\code{\link{row}} to get rows.
}
\examples{
# extract an off-diagonal of a matrix
ma <- matrix(1:12, 3,4)
ma[row(ma)==col(ma) + 1]

# create an identity 5-by-5 matrix
x <- matrix(0,nr=5,nc=5)
x[row(x)==col(x)] <- 1
}
\keyword{array}
