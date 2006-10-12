\name{svd}
\alias{svd}
\alias{La.svd}
\title{Singular Value Decomposition of a Matrix}
\usage{
svd(x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE)

La.svd(x, nu = min(n, p), nv = min(n, p))
}
\arguments{
  \item{x}{a real or complex matrix whose SVD decomposition is to be computed.}
  \item{nu}{the number of left  singular vectors to be computed.
    This must between \code{0} and \code{n = nrow(x)}.}
  \item{nv}{the number of right singular vectors to be computed.
    This must be between \code{0} and \code{p = ncol(x)}.}
  \item{LINPACK}{logical. Should LINPACK be used (for compatibility with
    \R < 1.7.0)?  In this case \code{nu} must be \code{0},
    \code{nrow(x)} or \code{ncol(x)}.}
}
\description{
  Compute the singular-value decomposition of a rectangular matrix.
}
\details{
  The singular value decomposition plays an important role in many
  statistical techniques.  \code{svd} and \code{La.svd} provide two
  slightly different interfaces.  The main functions used are
  the LAPACK routines DGESDD and ZGESVD; \code{svd(LINPACK = TRUE)}
  provides an interface to the LINPACK routine DSVDC, purely for
  backwards compatibility.
  
  Computing the singular vectors is the slow part for large matrices.
  The computation will be more efficient if \code{nu <= min(n, p)} and
  \code{nv <= min(n, p)}, and even more efficient if one or both are zero.

  Unsuccessful results from the underlying LAPACK code will result in an
  error giving a positive error code: these can only be interpreted by
  detailed study of the FORTRAN code.
}
\value{
  The SVD decomposition of the matrix as computed by LAPACK/LINPACK,
  \deqn{ \bold{X = U D V'},} where \eqn{\bold{U}} and \eqn{\bold{V}} are
  orthogonal, \eqn{\bold{V'}} means \emph{V transposed}, and
  \eqn{\bold{D}} is a diagonal matrix with the singular
  values \eqn{D_{ii}}{D[i,i]}.  Equivalently, \eqn{\bold{D = U' X V}},
  which is verified in the examples, below.

  The returned value is a list with components
  \item{d}{a vector containing the singular values of \code{x}, of
    length \code{min(n, p)}.}
  \item{u}{a matrix whose columns contain the left singular vectors of
    \code{x}, present if \code{nu > 0}.  Dimension \code{c(n, nu)}.}
  \item{v}{a matrix whose columns contain the right singular vectors of
    \code{x}, present if \code{nv > 0}.  Dimension \code{c(p, nv)}.}

  For \code{La.svd} the return value replaces \code{v} by \code{vt}, the
  (conjugated if complex) transpose of \code{v}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978)
  \emph{LINPACK Users Guide.}  Philadelphia: SIAM Publications.

  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition. SIAM.\cr
  Available on-line at
  \url{http://www.netlib.org/lapack/lug/lapack_lug.html}.
}
\seealso{
  \code{\link{eigen}}, \code{\link{qr}}.
}
\examples{
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
X <- hilbert(9)[,1:6]
(s <- svd(X))
D <- diag(s$d)
s$u \%*\% D \%*\% t(s$v) #  X = U D V'
t(s$u) \%*\% X \%*\% s$v #  D = U' X V
}
\keyword{algebra}
\keyword{array}

