% File src/library/base/man/solve.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{solve}
\title{Solve a System of Equations}
\description{
  This generic function solves the equation \code{a \%*\% x = b} for \code{x},
  where \code{b} can be either a vector or a matrix.
}
\usage{
solve(a, b, \dots)

\method{solve}{default}(a, b, tol, LINPACK = FALSE, \dots)
}
\alias{solve}
\alias{solve.default}
\arguments{
  \item{a}{a square numeric or complex matrix containing the coefficients of
    the linear system.  Logical matrices are coerced to numeric.}
  \item{b}{a numeric or complex vector or matrix giving the right-hand
    side(s) of the linear system.  If missing, \code{b} is taken to be
    an identity matrix and \code{solve} will return the inverse of \code{a}.}
  \item{tol}{the tolerance for detecting linear dependencies in the
    columns of \code{a}.  The default is \code{.Machine$double.eps}. Not
    currently used with complex matrices \code{a}.}
  \item{LINPACK}{logical.  Defunct and ignored.}
  \item{\dots}{further arguments passed to or from other methods}
}
\details{
  \code{a} or \code{b} can be complex, but this uses double complex
  arithmetic which might not be available on all platforms.

  The row and column names of the result are taken from the column names
  of \code{a} and of \code{b} respectively.  If \code{b} is missing the
  column names of the result are the row names of \code{a}.  No check is
  made that the column names of \code{a} and the row names of \code{b}
  are equal.

  For back-compatibility \code{a} can be a (real) QR decomposition,
  although \code{\link{qr.solve}} should be called in that case.
  \code{\link{qr.solve}} can handle non-square systems.

  Unsuccessful results from the underlying LAPACK code will result in an
  error giving a positive error code: these can only be interpreted by
  detailed study of the FORTRAN code.
}
\source{
  The default method is an interface to the LAPACK routines \code{DGESV}
  and \code{ZGESV}.

  LAPACK is from \url{http://www.netlib.org/lapack}.
}
\references{
  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition. SIAM.\cr
  Available on-line at
  \url{http://www.netlib.org/lapack/lug/lapack_lug.html}.

  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
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
