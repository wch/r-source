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
\item{values}{a vector containing the eigenvalues of \code{x}.}
\item{vectors}{a matrix whose columns contain the eigenvectors of \code{x}.}
}
\references{
Smith, B. T, J. M. Boyle, J. J. Dongarra, B. S. Garbow, Y. Ikebe,
V. Klema, C. B. Moler (1976).
\emph{Matrix Eigensystems Routines - EISPACK Guide}.
Springer-Verlag Lecture Notes in Computer Science.
}
\seealso{
\code{\link{svd}}, \code{\link{qr}}.
}
