\name{aperm}
\title{Array Transposition}
\usage{
aperm(a, perm, resize=TRUE)
}
\alias{aperm}
\arguments{
\item{a}{the array to be transposed.}
\item{perm}{the subscript permutation vector.}
\item{resize}{a flag indicating whether the vector
should be resized as well as having its elements reordered
(default \code{TRUE}.)}}
\value{
A transposed version of array \code{a}, with subscripts permuted as
indicated by the array \code{perm}.  If \code{resize} is \code{TRUE}, the
matrix is reshaped as well as having its elements permuted.

The function \code{t} provides a faster and more convenient
way of transposing matrices.
}
\seealso{
\code{\link{t}}.
}
\examples{
# interchange the first two subscripts on a 3-way array x
x  <- array(1:24, 2:4)
xt <- aperm(x, c(2,1,3))
all( t(xt[,,2]) == x[,,2] )
}

\keyword{array}
