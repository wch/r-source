\name{chol}
\alias{chol}
\title{The Choleski Decomposition}
\description{
  Compute the Choleski factorization of a symmetric (Hermitian),
  positive definite square matrix.
}
\usage{
chol(x)
}
\arguments{
  \item{x}{a symmetric, positive definite matrix.}
}
\value{
  The upper triangular factor of the Choleski decomposition, i.e., the
  matrix \eqn{R} such that \eqn{R'R = x} (see example).
}
\details{
  Note that only the upper triangular part of \code{x} is
  used such that the above only holds when \code{x} \emph{is} symmetric.

  If the matrix is not positive-definite an error is generated.
  A non-negative definite but not strictly positive-definite matrix
  may or may not give an error depending on the numerical errors.
}
\references{
  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978)
  \emph{LINPACK Users Guide.}  Philadelphia: SIAM Publications.
}
\seealso{
  \code{\link{chol2inv}} for its \emph{inverse},
  \code{\link{backsolve}} for solving linear systems with upper
  triangular left sides.

  \code{\link{qr}}, \code{\link{svd}} for related matrix factorizations.
}
\examples{
( m <- matrix(c(5,1,1,3),2,2) )
( cm <- chol(m) )
t(cm) \%*\% cm  #-- = 'm'
all(abs(m  -  t(cm) \%*\% cm) < 100* .Machine$double.eps) # TRUE
}
\keyword{algebra}
\keyword{array}
