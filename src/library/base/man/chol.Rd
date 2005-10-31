\name{chol}
\alias{chol}
\alias{La.chol}
\title{The Choleski Decomposition}
\description{
  Compute the Choleski factorization of a real symmetric
  positive-definite square matrix.
}
\usage{
chol(x, pivot = FALSE,  LINPACK = pivot)
La.chol(x)
}
\arguments{
  \item{x}{a real symmetric, positive-definite matrix}
  \item{pivot}{Should pivoting be used?}
  \item{LINPACK}{logical. Should LINPACK be used in the non-pivoting
    case (for compatibility with \R < 1.7.0)?}
}
\value{
  The upper triangular factor of the Choleski decomposition, i.e., the
  matrix \eqn{R} such that \eqn{R'R = x} (see example).

  If pivoting is used, then two additional attributes
  \code{"pivot"} and \code{"rank"} are also returned.
}
\details{
  \code{chol(pivot = TRUE)} provides an interface to the LINPACK routine DCHDC.
  \code{La.chol} provides an interface to the LAPACK routine DPOTRF.

  Note that only the upper triangular part of \code{x} is used, so
  that \eqn{R'R = x} when \code{x} is symmetric.

  If \code{pivot = FALSE} and \code{x} is not non-negative definite an
  error occurs.  If \code{x} is positive semi-definite (i.e., some zero
  eigenvalues) an error will also occur, as a numerical tolerance is used.

  If \code{pivot = TRUE}, then the Choleski decomposition of a positive
  semi-definite \code{x} can be computed.  The rank of \code{x} is
  returned as \code{attr(Q, "rank")}, subject to numerical errors.
  The pivot is returned as \code{attr(Q, "pivot")}.  It is no longer
  the case that \code{t(Q) \%*\% Q} equals \code{x}.  However, setting
  \code{pivot <- attr(Q, "pivot")} and \code{oo <- order(pivot)}, it
  is true that \code{t(Q[, oo]) \%*\% Q[, oo]} equals \code{x},
  or, alternatively, \code{t(Q) \%*\% Q} equals \code{x[pivot,
  pivot]}.  See the examples.
}

\section{Warning}{
  The code does not check for symmetry.

  If \code{pivot = TRUE} and \code{x} is not non-negative
  definite then there will be no error message but a meaningless
  result will occur.  So only use \code{pivot = TRUE} when \code{x} is
  non-negative definite by construction.
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
  \code{\link{chol2inv}} for its \emph{inverse} (without pivoting),
  \code{\link{backsolve}} for solving linear systems with upper
  triangular left sides.

  \code{\link{qr}}, \code{\link{svd}} for related matrix factorizations.
}

\examples{
( m <- matrix(c(5,1,1,3),2,2) )
( cm <- chol(m) )
t(cm) \%*\% cm  #-- = 'm'
crossprod(cm)  #-- = 'm'

# now for something positive semi-definite
x <- matrix(c(1:5, (1:5)^2), 5, 2)
x <- cbind(x, x[, 1] + 3*x[, 2])
m <- crossprod(x)
qr(m)$rank # is 2, as it should be

# chol() may fail, depending on numerical rounding:
# chol() unlike qr() does not use a tolerance.
try(chol(m))

(Q <- chol(m, pivot = TRUE)) # NB wrong rank here ... see Warning section.
## we can use this by
pivot <- attr(Q, "pivot")
oo <- order(pivot)
t(Q[, oo]) \%*\% Q[, oo] # recover m
}
\keyword{algebra}
\keyword{array}
