\name{eigen}
\alias{eigen}
\alias{La.eigen}
\title{Spectral Decomposition of a Matrix}
\usage{
eigen(x, symmetric, only.values = FALSE)
La.eigen(x, symmetric, only.values = FALSE,
         method = c("dsyevr", "dsyev"))
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
  \item{method}{The LAPACK routine to use in the real symmetric case.}
}
\description{
  Function \code{eigen} computes eigenvalues and eigenvectors by providing an
  interface to the EISPACK routines \code{RS}, \code{RG}, \code{CH}
  and \code{CG}.

  Function \code{La.eigen} uses the LAPACK routines DSYEV/DSYEVR,
  DGEEV, ZHEEV and ZGEEV.
}
\details{
  If \code{symmetric} is unspecified, the code attempts to
  determine if the matrix is symmetric up to plausible numerical
  inaccuracies.  It is faster and surer to set the value yourself.

  \code{La.eigen} is preferred to \code{eigen} for new projects, but
  its eigenvectors may differ in sign and (in the asymmetric case) in
  normalization. (They may also differ between methods
  and between platforms.)

  The LAPACK routine DSYEVR is usually substantially
  faster than DSYEV: see
  \url{http://www.cs.berkeley.edu/~demmel/DOE2000/Report0100.html}.
  Most benefits are seen with an optimized BLAS system.

  Computing the eigenvectors is the slow part for large matrices.

  Using \code{method="dsyevr"} requires IEEE 754 arithmetic.  Should
  this not be supported on your platform, \code{method="dsyev"} is
  used, with a warning.
}
\value{
  The spectral decomposition of \code{x} is returned
  as components of a list.
  \item{values}{a vector containing the \eqn{p} eigenvalues of \code{x},
    sorted in \emph{decreasing} order, according to \code{Mod(values)}
    if they are complex.
  }
  \item{vectors}{a \eqn{p\times p}{p * p} matrix whose columns contain the
    eigenvectors of \code{x}, or \code{NULL} if \code{only.values} is
    \code{TRUE}.
    
    For \code{eigen(, symmetric = FALSE)} the choice of length of the
    eigenvectors is not defined by LINPACK. In all other cases the
    vectors are normalized to unit length.

    Recall that the eigenvectors are only defined up to a constant: even
    when the length is specified they are still only defined up to a
    scalar of modulus one (the sign for real matrices).
  }
}
\references{
  Smith, B. T, Boyle, J. M., Dongarra, J. J., Garbow, B. S., Ikebe,Y.,
  Klema, V., and  Moler, C. B. (1976).
  \emph{Matrix Eigensystems Routines -- EISPACK Guide}.
  Springer-Verlag Lecture Notes in Computer Science.

  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition. SIAM.\cr
  Available on-line at
  \url{http://www.netlib.org/lapack/lug/lapack_lug.html}.
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

Meps <- .Machine$double.eps
set.seed(123, kind="default")  # force a particular seed
m <- matrix(round(rnorm(25),3), 5,5)
sm <- m + t(m) #- symmetric matrix
em <- eigen(sm); V <- em$vect
print(lam <- em$values) # ordered DEcreasingly

stopifnot(
 abs(sm \%*\% V - V \%*\% diag(lam))          < 60*Meps,
 abs(sm       - V \%*\% diag(lam) \%*\% t(V)) < 60*Meps)

##------- Symmetric = FALSE:  -- different to above : ---

em <- eigen(sm, symmetric = FALSE); V2 <- em$vect
print(lam2 <- em$values) # ordered decreasingly in ABSolute value !
                         # and V2 is not normalized (where V is):
print(i <- rev(order(lam2)))
stopifnot(abs(lam - lam2[i]) < 60 * Meps)

zapsmall(Diag <- t(V2) \%*\% V2) # orthogonal, but not normalized
print(norm2V <- apply(V2 * V2, 2, sum))
stopifnot( abs(1- norm2V / diag(Diag)) < 60*Meps)

V2n <- sweep(V2,2, STATS= sqrt(norm2V), FUN="/")## V2n are now Normalized EV
apply(V2n * V2n, 2, sum)
##[1] 1 1 1 1 1

## Both are now TRUE:
stopifnot(abs(sm \%*\% V2n - V2n \%*\% diag(lam2))            < 60*Meps,
          abs(sm         - V2n \%*\% diag(lam2) \%*\% t(V2n)) < 60*Meps)

## Re-ordered as with symmetric:
sV <- V2n[,i]
slam <- lam2[i]
all(abs(sm \%*\% sV -  sV \%*\% diag(slam))             < 60*Meps)
all(abs(sm        -  sV \%*\% diag(slam) \%*\% t(sV)) < 60*Meps)
## sV  *is* now equal to V  -- up to sign (+-) and rounding errors
all(abs(c(1 - abs(sV / V)))       <     1000*Meps) # TRUE (P ~ 0.95)

}
\keyword{algebra}
\keyword{array}
