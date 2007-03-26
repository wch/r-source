\name{chol2inv}
\alias{chol2inv}
\title{Inverse from Choleski Decomposition}
\description{
  Invert a symmetric, positive definite square matrix from its Choleski
  decomposition.
}
\usage{
chol2inv(x, size = NCOL(x), LINPACK = FALSE)
}
\arguments{
  \item{x}{a matrix.  The first \code{size} columns of the upper triangle
    contain the Choleski decomposition of the matrix to be inverted.}
  \item{size}{the number of columns of \code{x} containing the
    Choleski decomposition.}
  \item{LINPACK}{logical.  Should LINPACK be used (for compatibility with
    \R < 1.7.0)?}
}
\value{
  The inverse of the matrix whose Choleski decomposition was given.
}
\details{
  This is an interface to the LAPACK routine DPOTRI and the LINPACK
  routine DPODI.
}
\references{
  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978)
  \emph{LINPACK Users Guide}.
  Philadelphia: SIAM Publications.

  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition.
  SIAM.
  Available on-line at
  \url{http://www.netlib.org/lapack/lug/lapack_lug.html}.
}
\seealso{
  \code{\link{chol}}, \code{\link{solve}}.
}
\examples{
cma <- chol(ma  <- cbind(1, 1:3, c(1,3,7)))
ma \%*\% chol2inv(cma)
}
\keyword{algebra}
\keyword{array}
