\name{kronecker}
\title{Kronecker Product of Arrays}
\usage{
kronecker(X, Y, FUN = "*", \dots)
X \%x\% Y
}
\alias{kronecker}
\alias{\%x\%}

\arguments{
  \item{X}{vector or array.}
  \item{Y}{vector or array.}
  \item{FUN}{a function, possibly specified as character (string).}
  \item{\dots}{optional arguments to be passed to \code{FUN}.}
}

\description{
  Computes the generalised kronecker product of two arrays,
  \code{X} and \code{Y}.  \code{kronecker(X, Y)} returns an array
  \code{A} with dimensions \code{dim(X) * dim(Y)}.
}
\details{
  If \code{X} and \code{Y} do not have the same number of dimensions,
  the smaller array is padded with dimensions of size one.  \code{A}
  consists of submatrices constructed by taking \code{X} one term at a time
  and expanding that term as \code{FUN(x, Y, \dots)}.
    
  \code{\%x\%} is an \code{\link{.Alias}} for \code{kronecker} (where
  \code{FUN} is hardwired to \code{"*"}).
}
\references{
  Searle, Shayle R. (1982)
  \emph{Matrix Algebra Useful for Statistics}; John Wiley and Sons.
}
\author{Jonathan Rougier}

\seealso{\code{\link{outer}} on which \code{kronecker} is built and
    \code{\link{matmult}} for usual matrix multiplication.
}
\examples{
# simple scalar multiplication
( M <- matrix(1:6, ncol=2) )
all(kronecker(4, M) == 4 * M)

( A <- matrix(0:3, ncol=2) )
A \%x\% cbind(2:3)
# Block diagonal array:
kronecker(diag(3), M)
all(kronecker(diag(3), M) == diag(3) \%x\% M)
}
\keyword{array}
