\name{svd}
\title{Singular Value Decomposition of a Matrix}
\usage{
svd(x, nu=min(n,p), nv=min(n,p))
}
\alias{svd}
\arguments{
\item{x}{a matrix whose SVD decomposition is to be computed.}
\item{nu}{the number of left eigenvectors to be computed.
This must be one of \code{0}, \code{nrow(x)} and \code{ncol(x)}.}
\item{nv}{the number of right eigenvectors to be computed.
This must be one of \code{0}, and \code{ncol(x)}.}
}
\description{
\code{svd} provides an interface to the LINPACK routine DSVDC.
The singular value decompostion plays an important role in many
statistical techniques.
}
\value{
The SVD decomposition of the matrix
as computed by LINPACK.
The components in the returned value correspond directly
to the values returned by DSVDC. 
\item{d}{a vector containing the singular values of \code{x}.}
\item{u}{a matrix whose columns contain the left eigenvectors of \code{x}.}
\item{v}{a matrix whose columns contain the right eigenvectors of \code{x}.}
}
\references{
Dongarra, J. J., J. R. Bunch, C. B. Moler and G. W. Stewart (1978).
\emph{LINPACK Users Guide}, SIAM Publications, Philadelphia.
}
\seealso{\code{\link{eigen}}, \code{\link{qr}}.}
\keyword{algebra}
\keyword{array}
