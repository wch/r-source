\name{solve}
\title{Solve a System of Equations}
\usage{
solve(a, b, \dots)

\method{solve}{default}(a, b, tol, LINPACK = FALSE, \dots)
}
\alias{solve}
\alias{solve.default}
\arguments{
  \item{a}{a square numeric or complex matrix containing the coefficients of
    the linear system.}
  \item{b}{a numeric or complex vector or matrix giving the right-hand
    side(s) of the linear system.  If missing, \code{b} is taken to be
    an identity matrix and \code{solve} will return the inverse of \code{a}.}
  \item{tol}{the tolerance for detecting linear dependencies in the
    columns of \code{a}.  If \code{LINPACK} is \code{TRUE} the default
    is \code{1e-7}, otherwise it is \code{.Machine$double.eps}. Future
    versions of R may use a tighter tolerance. Not presently used with
    complex matrices \code{a}.}
  \item{LINPACK}{logical. Should LINPACK be used (for compatibility with
    \R < 1.7.0)?  Otherwise LAPACK is used.}
  \item{\dots}{further arguments passed to or from other methods}
}
\description{
  This generic function solves the equation \code{a \%*\% x = b} for \code{x},
  where \code{b} can be either a vector or a matrix.
}
\details{
  \code{a} or \code{b} can be complex, but this uses double complex
  arithmetic which might not be available on all platforms and LAPACK
  will always be used.

  The row and column names of the result are taken from the column
  names of \code{a} and of \code{b} respectively.  As from \R 1.7.0
  if \code{b} is missing the column names of the result are the row
  names of \code{a}.  No check is made that the column names of \code{a}
  and the row names of \code{b} are equal.

  For back-compatibility \code{a} can be a (real) QR decomposition,
  although \code{\link{qr.solve}} should be called in that case.
  \code{\link{qr.solve}} can handle non-square systems.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\seealso{
  \code{\link{solve.qr}} for the \code{qr} method,
  \code{\link{chol2inv}} for inverting from the Choleski factor
  \code{\link{backsolve}}, \code{\link{qr.solve}}.
}
\examples{
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8); h8
sh8 <- solve(h8)
round(sh8 \%*\% h8, 3)

A <- hilbert(4)
A[] <- as.complex(A)
## might not be supported on all platforms
try(solve(A))
}
\keyword{algebra}
