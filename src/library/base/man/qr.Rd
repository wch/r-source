\name{qr}
\title{The QR Decomposition of a Matrix}
\usage{
qr(x, tol=1e-07)
qr.coef(qr, y)
qr.qy(qr, y)
qr.qty(qr, y)
qr.resid(qr, y)
qr.fitted(qr, y, k = qr$rank)
qr.solve(a, b, tol = 1e-7)

is.qr(x)
as.qr(x)
}
\alias{qr}
\alias{qr.coef}
\alias{qr.qy}
\alias{qr.qty}
\alias{qr.resid}
\alias{qr.fitted}
\alias{qr.solve}
\alias{is.qr}
\alias{as.qr}
\arguments{
  \item{x}{a matrix whose QR decomposition is to be computed.}
  \item{tol}{the tolerance for detecting linear dependencies in the
    columns of \code{x}.}
  \item{qr}{a QR decomposition of the type computed by \code{qr}.}
  \item{y, b}{a vector or matrix of right-hand sides of equations.}
  \item{a}{A matrix or QR decomposition.}
}
\description{
  \code{qr} provides an interface to the techniques used in the LINPACK
  routine DQRDC.  The QR decomposition plays an important role in many
  statistical techniques.  In particular it can be used to solve the
  equation \eqn{\bold{Ax} = \bold{b}} for given matrix \eqn{\bold{A}},
  and vector \eqn{\bold{b}}.  It is useful for computing regression
  coefficients and in applying the Newton-Raphson algorithm.

  The functions \code{qr.coef}, \code{qr.resid}, and \code{qr.fitted}
  return the coefficients, residuals and fitted values obtained when
  fitting \code{y} to the matrix with QR decomposition \code{qr}.
  \code{qr.qy} and \code{qr.qty} return \code{Q \%*\% y} and
  \code{t(Q) \%*\% y}, where \code{Q} is the \eqn{\bold{Q}} matrix.

  \code{qr.solve} solves systems of equations via the QR decomposition.

  \code{is.qr} returns \code{TRUE} if \code{x} is a list with a
  component named \code{qr} and \code{FALSE} otherwise.

  It is not possible to coerce objects to mode \code{"qr"}.  Objects
  either are QR decompositions or they are not.
}
\value{
  The QR decomposition of the matrix as computed by LINPACK.
  The components in the returned value correspond directly
  to the values returned by DQRDC.
  \item{qr}{a matrix with the same dimensions as \code{x}.
    The upper triangle contains the \eqn{\bold{R}} of the decomposition
    and the lower triangle contains information on the \eqn{\bold{Q}} of
    the decomposition (stored in compact form).}
  \item{qraux}{a vector of length \code{ncol(x)} which contains
    additional information on \eqn{\bold{Q}}.}
  \item{rank}{the rank of \code{x} as computed by the decomposition.}
  \item{pivot}{information on the pivoting strategy used during
    the decomposition.}
}
\references{
  Dongarra, J. J., J. R. Bunch, C. B. Moler and G. W. Stewart (1978).
  \emph{LINPACK Users Guide}.
  Philadelphia, PA:  SIAM Publications.
}
\seealso{
  \code{\link{qr.Q}},  \code{\link{qr.R}},  \code{\link{qr.X}} for
  reconstruction of the matrices.
  \code{\link{solve.qr}},  \code{\link{lsfit}},
  \code{\link{eigen}}, \code{\link{svd}}.}
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
\keyword{regression}
