\name{svd}
\alias{svd}
\alias{La.svd}
\title{Singular Value Decomposition of a Matrix}
\usage{
svd(x, nu = min(n, p), nv = min(n, p))
La.svd(x, nu = min(n, p), nv = min(n, p), method = c("dgesdd", "dgesvd"))
}
\arguments{
  \item{x}{a matrix whose SVD decomposition is to be computed.}
  \item{nu}{the number of left  singular vectors to be computed.
    This must be one of \code{0}, \code{nrow(x)} and \code{ncol(x)},
    except for \code{method = "dgesdd"}.}
  \item{nv}{the number of right singular vectors to be computed.
    This must be one of \code{0} and \code{ncol(x)}.}
  \item{method}{The LAPACK routine to use in the real case.}
}
\description{
  Compute the singular-value decomposition of a rectangular matrix.
}
\details{
  The singular value decomposition plays an important role in many
  statistical techniques.

  \code{svd} provides an interface to the LINPACK routine DSVDC.
  \code{La.svd} provides an interface to the LAPACK routines DGESVD and
  DGESDD.  The latter is usually substantially faster if singular
  vectors are required: see
  \url{http://www.cs.berkeley.edu/~demmel/DOE2000/Report0100.html}.
  Most benefit is seen with an optimized BLAS system.

  \code{La.svd} is preferred to \code{svd} for new projects, but it is
  not an exact replacement as it returns the transpose of the right
  singular vector matrix, and the signs of the singular vectors may differ
  from those given by \code{svd}.  (They may also differ between methods
  and between platforms.)

  Both functions handle complex matrices via LAPACK routine ZGESVD.

  Computing the singular vectors is the slow part for large matrices.

  Using \code{method="dgesdd"} requires IEEE 754 arithmetic.  Should
  this not be supported on your platform, \code{method="dgesvd"} is
  used, with a warning.
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
  \item{u}{a matrix whose columns contain the left singular vectors of
    \code{x}.}
  \item{v}{a matrix whose columns contain the right singular vectors of
    \code{x}.}

  For \code{La.svd} the return value replaces \code{v} by \code{vt}, the
  (conjugated if complex) transpose of \code{v}.
}
\references{
  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978)
  \emph{LINPACK Users Guide.}  Philadelphia: SIAM Publications.

  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition. SIAM.\cr
  Available on-line at
  \url{http://www.netlib.org/lapack/lug/lapack_lug.html}.
}
\seealso{
  \code{\link{eigen}}, \code{\link{qr}}.

  \code{\link{capabilities}} to test for IEEE 754 arithmetic.
}
\examples{
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
str(X <- hilbert(9)[,1:6])
str(s <- svd(X))
Eps <- 100 * .Machine$double.eps

D <- diag(s$d)
stopifnot(abs(X - s$u \%*\% D \%*\% t(s$v)) < Eps)#  X = U D V'
stopifnot(abs(D - t(s$u) \%*\% X \%*\% s$v) < Eps)#  D = U' X V

X <- cbind(1, 1:7)
str(s <- svd(X)); D <- diag(s$d)
stopifnot(abs(X - s$u \%*\% D \%*\% t(s$v)) < Eps)#  X = U D V'
stopifnot(abs(D - t(s$u) \%*\% X \%*\% s$v) < Eps)#  D = U' X V

}
\keyword{algebra}
\keyword{array}

