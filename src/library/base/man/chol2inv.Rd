\name{chol2inv}
\alias{chol2inv}
\title{Inverse from Choleski Decomposition}
\description{
  Invert a symmetric, positive definite square matrix from its Choleski
  decomposition.
}
\usage{
chol2inv(x, size = ncol(x))
}
\arguments{
  \item{x}{a matrix.  The first \code{nc} columns of the upper triangle
    contain the Choleski decomposition of the matrix to be inverted.}
  \item{size}{the number of columns of \code{x} containing the
    Choleski decomposition.}
}
\value{
  The inverse of the decomposed matrix.
}
\references{
  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978)
  \emph{LINPACK Users Guide}.  Philadelphia: SIAM Publications.
}
\seealso{\code{\link{chol}}, \code{\link{solve}}.}
\examples{
cma <- chol(ma  <- cbind(1, 1:3, c(1,3,7)))
t(cma) \%*\% cma # = ma
all.equal(diag(3), ma \%*\% chol2inv(cma))
}
\keyword{algebra}
\keyword{array}
