\name{row}
\title{Row Indexes}
\usage{
row(x, as.factor=FALSE)
}
\alias{row}
\arguments{
\item{x}{a matrix.}
\item{as.factor}{a logical value indicating whether the
value should be returned as a factor rather than as numeric.}
}
\value{
An integer matrix with the same dimensions as \code{x} and whose
\code{ij}-th element is equal to \code{i}.
}
\seealso{
\code{\link{col}} to get columns.
}
\examples{
x <- matrix(1:12, 3, 4)
# extract the diagonal of a matrix
dx <- x[row(x)==col(x)]
dx

# create an identity 5-by-5 matrix
x <- matrix(0,nr=5,nc=5)
x[row(x)==col(x)] <- 1
x
}
\keyword{array}
