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
This function provides an interface to the EISPACK routines
\code{RS}, \code{RG}, \code{CH} and \code{CG}.
}
\value{
The spectral decomposition of \code{x} is returned
as components of a list.
\item{values}{a vector containing the eigenvalues of \code{x}, sorted
  \emph{decreasingly}, according to \code{Mod(values)} if they are complex.}
\item{vectors}{a matrix whose columns contain the eigenvectors of \code{x}.}
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
}
\keyword{algebra}
\keyword{array}
