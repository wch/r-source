\name{eigen}
\alias{eigen}
\title{Spectral Decomposition of a Matrix}
\usage{
eigen(x, symmetric, only.values = FALSE, EISPACK = FALSE)
}
\arguments{
  \item{x}{a matrix whose spectral decomposition is to be computed.}
  \item{symmetric}{if \code{TRUE}, the matrix is assumed to be symmetric
    (or Hermitian if complex) and only its lower triangle is used.
    If \code{symmetric} is not specified, the matrix is inspected for
    symmetry.}
  \item{only.values}{if \code{TRUE}, only the eigenvalues are computed
    and returned, otherwise both eigenvalues and eigenvectors are
    returned.}
  \item{EISPACK}{logical. Should EISPACK be used (for compatibility with
    \R < 1.7.0)?}
}
\description{
  Computes eigenvalues and eigenvectors of real or complex matrices.
}
\details{
  By default \code{eigen} uses the LAPACK routines DSYEVR,
  DGEEV, ZHEEV and ZGEEV whereas \code{eigen(EISPACK=TRUE)} provides an
  interface to the EISPACK routines \code{RS}, \code{RG}, \code{CH}
  and \code{CG}.
   
  If \code{symmetric} is unspecified, the code attempts to
  determine if the matrix is symmetric up to plausible numerical
  inaccuracies.  It is faster and surer to set the value yourself.

  \code{eigen} is preferred to \code{eigen(EISPACK = TRUE)}
  for new projects, but its eigenvectors may differ in sign and
  (in the asymmetric case) in normalization. (They may also differ
  between methods and between platforms.)

  Computing the eigenvectors is the slow part for large matrices.

  Computing the eigendecomposition of a matrix is subject to errors on a
  real-world computer: the definitive analysis is Wilkinson (1965).  All
  you can hope for is a solution to a problem suitably close to
  \code{x}.  So even though a real asymmetric \code{x} may have an
  algebraic solution with repeated real eigenvalues, the computed
  solution may be of a similar matrix with complex conjugate pairs of
  eigenvalues.
}
\value{
  The spectral decomposition of \code{x} is returned as components of a
  list with components
  
  \item{values}{a vector containing the \eqn{p} eigenvalues of \code{x},
    sorted in \emph{decreasing} order, according to \code{Mod(values)}
    in the asymmetric case when they might be complex (even for real
    matrices).  For real asymmetric matrices the vector will be
    complex only if complex conjugate pairs of eigenvalues are detected.
  }
  \item{vectors}{either a \eqn{p\times p}{p * p} matrix whose columns
    contain the eigenvectors of \code{x}, or \code{NULL} if
    \code{only.values} is \code{TRUE}.
    
    For \code{eigen(, symmetric = FALSE, EISPACK =TRUE)} the choice of
    length of the eigenvectors is not defined by EISPACK.  In all other
    cases the vectors are normalized to unit length.

    Recall that the eigenvectors are only defined up to a constant: even
    when the length is specified they are still only defined up to a
    scalar of modulus one (the sign for real matrices).
  }
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Smith, B. T, Boyle, J. M., Dongarra, J. J., Garbow, B. S., Ikebe,Y.,
  Klema, V., and  Moler, C. B. (1976).
  \emph{Matrix Eigensystems Routines -- EISPACK Guide}.
  Springer-Verlag Lecture Notes in Computer Science.

  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition. SIAM.\cr
  Available on-line at
  \url{http://www.netlib.org/lapack/lug/lapack_lug.html}.

  Wilkinson, J. H. (1965) \emph{The Algebraic Eigenvalue Problem.}
  Clarendon Press, Oxford.
}

\seealso{
  \code{\link{svd}}, a generalization of \code{eigen}; \code{\link{qr}}, and
  \code{\link{chol}} for related decompositions.

  To compute the determinant of a matrix, the \code{\link{qr}}
  decomposition is much more efficient: \code{\link{det}}.

  \code{\link{capabilities}} to test for IEEE 754 arithmetic.
}
\examples{
eigen(cbind(c(1,-1),c(-1,1)))
eigen(cbind(c(1,-1),c(-1,1)), symmetric = FALSE)# same (different algorithm).

eigen(cbind(1,c(1,-1)), only.values = TRUE)
eigen(cbind(-1,2:1)) # complex values
eigen(print(cbind(c(0,1i), c(-1i,0))))# Hermite ==> real Eigen values
## 3 x 3:
eigen(cbind( 1,3:1,1:3))
eigen(cbind(-1,c(1:2,0),0:2)) # complex values

}
\keyword{algebra}
\keyword{array}
