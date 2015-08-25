% File src/library/base/man/chol.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{chol}
\alias{chol}
\alias{chol.default}
\title{The Choleski Decomposition}
\description{
  Compute the Choleski factorization of a real symmetric
  positive-definite square matrix.
}
\usage{
chol(x, \dots)

\method{chol}{default}(x, pivot = FALSE,  LINPACK = FALSE, tol = -1, \dots)
}
\arguments{
  \item{x}{an object for which a method exists.  The default method
    applies to numeric (or logical) symmetric, positive-definite matrices.}
  \item{\dots}{arguments to be based to or from methods.}
  \item{pivot}{Should pivoting be used?}
  \item{LINPACK}{logical.  Should LINPACK be used (now ignored)?}
  \item{tol}{A numeric tolerance for use with \code{pivot = TRUE}.}
}
\value{
  The upper triangular factor of the Choleski decomposition, i.e., the
  matrix \eqn{R} such that \eqn{R'R = x} (see example).

  If pivoting is used, then two additional attributes
  \code{"pivot"} and \code{"rank"} are also returned.
}
\details{
  \code{chol} is generic: the description here applies to the default
  method.

  Note that only the upper triangular part of \code{x} is used, so
  that \eqn{R'R = x} when \code{x} is symmetric.

  If \code{pivot = FALSE} and \code{x} is not non-negative definite an
  error occurs.  If \code{x} is positive semi-definite (i.e., some zero
  eigenvalues) an error will also occur as a numerical tolerance is used.

  If \code{pivot = TRUE}, then the Choleski decomposition of a positive
  semi-definite \code{x} can be computed.  The rank of \code{x} is
  returned as \code{attr(Q, "rank")}, subject to numerical errors.
  The pivot is returned as \code{attr(Q, "pivot")}.  It is no longer
  the case that \code{t(Q) \%*\% Q} equals \code{x}.  However, setting
  \code{pivot <- attr(Q, "pivot")} and \code{oo <- order(pivot)}, it
  is true that \code{t(Q[, oo]) \%*\% Q[, oo]} equals \code{x},
  or, alternatively, \code{t(Q) \%*\% Q} equals \code{x[pivot,
    pivot]}.  See the examples.
  
  The value of \code{tol} is passed to LAPACK, with negative values
  selecting the default tolerance of (usually) \code{nrow(x) *
  .Machine$double.neg.eps * max(diag(x)}.  The algorithm terminates once
  the pivot is less than \code{tol}.

  Unsuccessful results from the underlying LAPACK code will result in an
  error giving a positive error code: these can only be interpreted by
  detailed study of the FORTRAN code.
}

\section{Warning}{
  The code does not check for symmetry.

  If \code{pivot = TRUE} and \code{x} is not non-negative definite then
  there will be a warning message but a meaningless result will occur.
  So only use \code{pivot = TRUE} when \code{x} is non-negative definite
  by construction.
}

\source{
  This is an interface to the LAPACK routines \code{DPOTRF} and
  \code{DPSTRF},

  LAPACK is from \url{http://www.netlib.org/lapack} and its guide is listed
  in the references.
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
colnames(x) <- letters[20:22]
m <- crossprod(x)
qr(m)$rank # is 2, as it should be

# chol() may fail, depending on numerical rounding:
# chol() unlike qr() does not use a tolerance.
try(chol(m))

(Q <- chol(m, pivot = TRUE))
## we can use this by
pivot <- attr(Q, "pivot")
crossprod(Q[, order(pivot)]) # recover m

## now for a non-positive-definite matrix
( m <- matrix(c(5,-5,-5,3), 2, 2) )
try(chol(m))  # fails
(Q <- chol(m, pivot = TRUE)) # warning
crossprod(Q)  # not equal to m
}
\keyword{algebra}
\keyword{array}
