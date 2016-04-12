% File src/library/base/man/eigen.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{eigen}
\alias{eigen}
\concept{eigen vector}
\concept{eigen value}
\title{Spectral Decomposition of a Matrix}
\description{
  Computes eigenvalues and eigenvectors of numeric (double, integer,
  logical) or complex matrices.
}
\usage{
eigen(x, symmetric, only.values = FALSE, EISPACK = FALSE)
}
\arguments{
  \item{x}{a numeric or complex matrix whose spectral decomposition is to
    be computed.  Logical matrices are coerced to numeric.}
  \item{symmetric}{if \code{TRUE}, the matrix is assumed to be symmetric
    (or Hermitian if complex) and only its lower triangle (diagonal
    included) is used.  If \code{symmetric} is not specified,
    \code{\link{isSymmetric}(x)} is used.}
  \item{only.values}{if \code{TRUE}, only the eigenvalues are computed
    and returned, otherwise both eigenvalues and eigenvectors are
    returned.}
  \item{EISPACK}{logical. Defunct and ignored.}
}
\details{
  If \code{symmetric} is unspecified, \code{\link{isSymmetric}(x)}
  determines if the matrix is symmetric up to plausible numerical
  inaccuracies.  It is surer and typically much faster to set the value
  yourself.

  Computing the eigenvectors is the slow part for large matrices.

  Computing the eigendecomposition of a matrix is subject to errors on a
  real-world computer: the definitive analysis is Wilkinson (1965).  All
  you can hope for is a solution to a problem suitably close to
  \code{x}.  So even though a real asymmetric \code{x} may have an
  algebraic solution with repeated real eigenvalues, the computed
  solution may be of a similar matrix with complex conjugate pairs of
  eigenvalues.

  Unsuccessful results from the underlying LAPACK code will result in an
  error giving a positive error code (most often \code{1}): these can
  only be interpreted by detailed study of the FORTRAN code.
}
\value{
  The spectral decomposition of \code{x} is returned as a list with components

  \item{values}{a vector containing the \eqn{p} eigenvalues of \code{x},
    sorted in \emph{decreasing} order, according to \code{Mod(values)}
    in the asymmetric case when they might be complex (even for real
    matrices).  For real asymmetric matrices the vector will be
    complex only if complex conjugate pairs of eigenvalues are detected.
  }
  \item{vectors}{either a \eqn{p\times p}{p * p} matrix whose columns
    contain the eigenvectors of \code{x}, or \code{NULL} if
    \code{only.values} is \code{TRUE}.  The vectors are normalized to
    unit length.

    Recall that the eigenvectors are only defined up to a constant: even
    when the length is specified they are still only defined up to a
    scalar of modulus one (the sign for real matrices).
  }

  When \code{only.values} is not true, as by default, the result is of
  S3 class \code{"eigen"}.

  If \code{r <- eigen(A)}, and \code{V <- r$vectors; lam <- r$values},
  then \deqn{A = V \Lambda V^{-1}}{A = V Lmbd V^(-1)} (up to numerical
  fuzz), where \eqn{\Lambda =}{Lmbd =}\code{diag(lam)}.
}
\source{
  \code{eigen} uses the LAPACK routines \code{DSYEVR}, \code{DGEEV},
  \code{ZHEEV} and \code{ZGEEV}.

  LAPACK is from \url{http://www.netlib.org/lapack} and its guide is listed
  in the references.
}
\references{
  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}.  Third Edition.  SIAM.\cr
  Available on-line at
  \url{http://www.netlib.org/lapack/lug/lapack_lug.html}.

  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
  Springer-Verlag Lecture Notes in Computer Science \bold{6}.

  Wilkinson, J. H. (1965) \emph{The Algebraic Eigenvalue Problem.}
  Clarendon Press, Oxford.
}

\seealso{
  \code{\link{svd}}, a generalization of \code{eigen}; \code{\link{qr}}, and
  \code{\link{chol}} for related decompositions.

  To compute the determinant of a matrix, the \code{\link{qr}}
  decomposition is much more efficient: \code{\link{det}}.
}
\examples{
eigen(cbind(c(1,-1), c(-1,1)))
eigen(cbind(c(1,-1), c(-1,1)), symmetric = FALSE)
# same (different algorithm).

eigen(cbind(1, c(1,-1)), only.values = TRUE)
eigen(cbind(-1, 2:1)) # complex values
eigen(print(cbind(c(0, 1i), c(-1i, 0)))) # Hermite ==> real Eigenvalues
## 3 x 3:
eigen(cbind( 1, 3:1, 1:3))
eigen(cbind(-1, c(1:2,0), 0:2)) # complex values

}
\keyword{algebra}
\keyword{array}
