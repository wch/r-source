\name{qr}
\alias{qr}
\alias{qr.coef}
\alias{qr.qy}
\alias{qr.qty}
\alias{qr.resid}
\alias{qr.fitted}
\alias{qr.solve}
\alias{is.qr}
\alias{as.qr}
\alias{solve.qr}
\title{The QR Decomposition of a Matrix}
\usage{
qr(x, tol = 1e-07 , LAPACK = FALSE)
qr.coef(qr, y)
qr.qy(qr, y)
qr.qty(qr, y)
qr.resid(qr, y)
qr.fitted(qr, y, k = qr$rank)
qr.solve(a, b, tol = 1e-7)
\method{solve}{qr}(a, b, \dots)

is.qr(x)
as.qr(x)
}
\arguments{
  \item{x}{a matrix whose QR decomposition is to be computed.}
  \item{tol}{the tolerance for detecting linear dependencies in the
    columns of \code{x}. Only used if \code{LAPACK} is false and
    \code{x} is real.}
  \item{qr}{a QR decomposition of the type computed by \code{qr}.}
  \item{y, b}{a vector or matrix of right-hand sides of equations.}
  \item{a}{A QR decomposition or (\code{qr.solve} only) a rectangular matrix.}
  \item{k}{effective rank.}
  \item{LAPACK}{logical. For real \code{x}, if true use LAPACK
    otherwise use LINPACK.}
  \item{\dots}{further arguments passed to or from other methods}
}
\description{
  \code{qr} computes the QR decomposition of a matrix.  It provides an
  interface to the techniques used in the LINPACK routine DQRDC
  or the LAPACK routines DGEQP3 and (for complex matrices) ZGEQP3.
}
\details{
  The QR decomposition plays an important role in many
  statistical techniques.  In particular it can be used to solve the
  equation \eqn{\bold{Ax} = \bold{b}} for given matrix \eqn{\bold{A}},
  and vector \eqn{\bold{b}}.  It is useful for computing regression
  coefficients and in applying the Newton-Raphson algorithm.

  The functions \code{qr.coef}, \code{qr.resid}, and \code{qr.fitted}
  return the coefficients, residuals and fitted values obtained when
  fitting \code{y} to the matrix with QR decomposition \code{qr}.
  (If pivoting is used, some of the coefficients will be \code{NA}.)
  \code{qr.qy} and \code{qr.qty} return \code{Q \%*\% y} and
  \code{t(Q) \%*\% y}, where \code{Q} is the (complete) \eqn{\bold{Q}} matrix.

  All the above functions keep \code{dimnames} (and \code{names}) of
  \code{x} and \code{y} if there are.

  \code{solve.qr} is the method for \code{\link{solve}} for \code{qr} objects.
  \code{qr.solve} solves systems of equations via the QR decomposition:
  if \code{a} is a QR decomposition it is the same as \code{solve.qr},
  but if \code{a} is a rectangular matrix the QR decomposition is
  computed first.  Either will handle over- and under-determined
  systems, providing a least-squares fit if appropriate.

  \code{is.qr} returns \code{TRUE} if \code{x} is a \code{\link{list}}
  with components named \code{qr}, \code{rank} and \code{qraux} and
  \code{FALSE} otherwise.

  It is not possible to coerce objects to mode \code{"qr"}.  Objects
  either are QR decompositions or they are not.
}
\value{
  The QR decomposition of the matrix as computed by LINPACK or LAPACK.
  The components in the returned value correspond directly
  to the values returned by DQRDC/DGEQP3/ZGEQP3.
  \item{qr}{a matrix with the same dimensions as \code{x}.
    The upper triangle contains the \eqn{\bold{R}} of the decomposition
    and the lower triangle contains information on the \eqn{\bold{Q}} of
    the decomposition (stored in compact form).  Note that the storage
    used by DQRDC and DGEQP3 differs.}
  \item{qraux}{a vector of length \code{ncol(x)} which contains
    additional information on \eqn{\bold{Q}}.}
  \item{rank}{the rank of \code{x} as computed by the decomposition:
    always full rank in the LAPACK case.}
  \item{pivot}{information on the pivoting strategy used during
    the decomposition.}

  Non-complex QR objects computed by LAPACK have the attribute
  \code{"useLAPACK"} with value \code{TRUE}.
}
\note{
  To compute the determinant of a matrix (do you \emph{really} need it?),
  the QR decomposition is much more efficient than using Eigen values
  (\code{\link{eigen}}).  See \code{\link{det}}.

  Using LAPACK (including in the complex case) uses column pivoting and
  does not attempt to detect rank-deficient matrices.
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
  \code{\link{qr.Q}},  \code{\link{qr.R}},  \code{\link{qr.X}} for
  reconstruction of the matrices.
  \code{\link{lm.fit}},  \code{\link{lsfit}},
  \code{\link{eigen}}, \code{\link{svd}}.

  \code{\link{det}} (using \code{qr}) to compute the determinant of a matrix.
}
\examples{
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h9 <- hilbert(9); h9
qr(h9)$rank           #--> only 7
qrh9 <- qr(h9, tol = 1e-10)
qrh9$rank             #--> 9
##-- Solve linear equation system  H \%*\% x = y :
y <- 1:9/10
x <- qr.solve(h9, y, tol = 1e-10) # or equivalently :
x <- qr.coef(qrh9, y) #-- is == but much better than
                      #-- solve(h9) \%*\% y
h9 \%*\% x              # = y
}
\keyword{algebra}
\keyword{array}
