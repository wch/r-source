\name{chol2inv}
\alias{chol2inv}
\alias{La.chol2inv}
\title{Inverse from Choleski Decomposition}
\description{
  Invert a symmetric, positive definite square matrix from its Choleski
  decomposition.
}
\usage{
chol2inv(x, size = ncol(x), LINPACK = FALSE)
La.chol2inv(x, size = ncol(x))
}
\arguments{
  \item{x}{a matrix.  The first \code{nc} columns of the upper triangle
    contain the Choleski decomposition of the matrix to be inverted.}
  \item{size}{the number of columns of \code{x} containing the
    Choleski decomposition.}
  \item{LINPACK}{logical. Should LINPACK be used (for compatibility with
    \R < 1.7.0)?}
}
\value{
  The inverse of the decomposed matrix.
}
\details{
  \code{chol2inv(LINPACK=TRUE)} provides an interface to the LINPACK routine DPODI.
  \code{La.chol2inv} provides an interface to the LAPACK routine DPOTRI.
}
\references{
  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978)
  \emph{LINPACK Users Guide}.  Philadelphia: SIAM Publications.

  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition. SIAM.\cr
  Available on-line at
  \url{http://www.netlib.org/lapack/lug/lapack_lug.html}.
}
\seealso{\code{\link{chol}}, \code{\link{solve}}.}
\examples{
cma <- chol(ma  <- cbind(1, 1:3, c(1,3,7)))
ma \%*\% chol2inv(cma)
}
\keyword{algebra}
\keyword{array}
