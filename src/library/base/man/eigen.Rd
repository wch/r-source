\name{eigen}
\title{Spectral Decomposition of a Matrix}
\usage{
eigen(x, symmetric, only.values=FALSE)
}
\alias{eigen}
\arguments{
\item{x}{a matrix whose spectral decomposition is to be computed.}
\item{symmetric}{if \code{TRUE}, the matrix is assumed to be symmetric
    (or Hermitian if complex) and only its lower triangle is used.
    If \code{symmetric} is not specified, the matrix is inspected for symmetry.}
\item{only.values}{if \code{TRUE}, only the eigenvalues are computed
    and returned, otherwise both eigenvalues and eigenvectors are
    returned.}
}
\description{
This function computes eigenvalues and eigenvectors by providing an
interface to the EISPACK routines \code{RS}, \code{RG}, \code{CH} and \code{CG}.
}
\value{
The spectral decomposition of \code{x} is returned
as components of a list.
\item{values}{a vector containing the \eqn{p} eigenvalues of \code{x}, sorted
  \emph{decreasingly}, according to \code{Mod(values)} if they are complex.}
\item{vectors}{a \eqn{p\times p}{p * p} matrix whose columns contain the
    eigenvectors of \code{x}, or \code{NULL} if \code{only.values} is
    \code{TRUE}.}
}
\note{To compute the determinant of a matrix (do you \emph{really} need it?),
    it is much more efficient to use the QR decomposition, see \code{\link{qr}}.
}
\references{
Smith, B. T, J. M. Boyle, J. J. Dongarra, B. S. Garbow, Y. Ikebe,
V. Klema, C. B. Moler (1976).
\emph{Matrix Eigensystems Routines - EISPACK Guide}.
Springer-Verlag Lecture Notes in Computer Science.
}
\seealso{
\code{\link{svd}}, a generalization of \code{eigen}; \code{\link{qr}}, and
\code{\link{chol}} for related decompositions.
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

Meps <- .Alias(.Machine$double.eps)
m <- matrix(round(rnorm(25),3), 5,5)
sm <- m + t(m) #- symmetric matrix
em <- eigen(sm); V <- em$vect
print(lam <- em$values) # ordered DEcreasingly

all(abs(sm \%*\% V - V \%*\% diag(lam))          < 60*Meps)
all(abs(sm       - V \%*\% diag(lam) \%*\% t(V)) < 60*Meps)

##------- Symmetric = FALSE:  -- different to above : ---

em <- eigen(sm, symmetric = FALSE); V2 <- em$vect
print(lam2 <- em$values) # ordered decreasingly in ABSolute value !
			 # and V2 is not normalized (where V is):
print(i <- rev(order(lam2)))
all(abs(1 - lam2[i] / lam) < 60 * Meps)# [1] TRUE

zapsmall(Diag <- t(V2) \%*\% V2) # orthogonal, but not normalized
print(norm2V <- apply(V2 * V2, 2, sum))
all( abs(1- norm2V / diag(Diag)) < 60*Meps) #> TRUE

V2n <- sweep(V2,2, STATS= sqrt(norm2V), FUN="/")## V2n are now Normalized EV
apply(V2n * V2n, 2, sum)
##[1] 1 1 1 1 1

## Both are now TRUE:
all(abs(sm \%*\% V2n - V2n \%*\% diag(lam2))            < 60*Meps)
all(abs(sm         - V2n \%*\% diag(lam2) \%*\% t(V2n)) < 60*Meps)

## Re-ordered as with symmetric:
sV <- V2n[,i]
slam <- lam2[i]
all(abs(sm \%*\% sV -  sV \%*\% diag(slam))             < 60*Meps)
all(abs(sm        -  sV \%*\% diag(slam) \%*\% t(sV)) < 60*Meps)
## sV  *is* now equal to V  -- up to sign (+-) and rounding errors
all(abs(c(1 - abs(sV / V)))       < 	1000*Meps) # TRUE (P ~ 0.95)
}
\keyword{algebra}
\keyword{array}
