\name{svd}
\title{Singular Value Decomposition of a Matrix}
\usage{
svd(x, nu = min(n,p), nv = min(n,p))
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
  Compute the singular-value decomposition of a rectangular matrix.
}
\details{
  \code{svd} provides an interface to the LINPACK routine DSVDC.
  The singular value decomposition plays an important role in many
  statistical techniques.
}
\value{
  The SVD decomposition of the matrix as computed by LINPACK,
  \deqn{ \bold{X = U D V'},} where \eqn{\bold{U}} and \eqn{\bold{V}} are
  orthogonal, \eqn{\bold{V'}} means \emph{V transposed}, and
  \eqn{\bold{D}} is a diagonal matrix with the singular
  values \eqn{D_{ii}}{D[i,i]}.  Equivalently, \eqn{\bold{D = U' X V}},
  which is verified in the examples, below.

  The components in the returned value correspond directly
  to the values returned by DSVDC.
  \item{d}{a vector containing the singular values of \code{x}.}
  \item{u}{a matrix whose columns contain the left eigenvectors of \code{x}.}
  \item{v}{a matrix whose columns contain the right eigenvectors of \code{x}.}
}
\references{
  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978)
  \emph{LINPACK Users Guide.}  Philadelphia: SIAM Publications.
}
\seealso{\code{\link{eigen}}, \code{\link{qr}}.}
\examples{
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
str(X <- hilbert(9)[,1:6])
str(s <- svd(X))
Eps <- 10 * .Machine$double.eps

D <- diag(s$d)
all(abs(X - s$u \%*\% D \%*\% t(s$v)) < Eps)# TRUE:  X = U D V'
all(abs(D - t(s$u) \%*\% X \%*\% s$v) < Eps)# TRUE:  D = U' X V

X <- cbind(1, 1:7)
str(s <- svd(X)); D <- diag(s$d)
all(abs(X - s$u \%*\% D \%*\% t(s$v)) < Eps)# TRUE:  X = U D V'
all(abs(D - t(s$u) \%*\% X \%*\% s$v) < Eps)# TRUE:  D = U' X V
}
\keyword{algebra}
\keyword{array}
