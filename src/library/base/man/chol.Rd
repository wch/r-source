\name{chol}
\title{The Choleski Decomposition}
\usage{
chol(x)
}
\alias{chol}
\arguments{
\item{x}{a symmetric positive-definite matrix.}
}
\value{
This function computes the Choleski factorization
of the square matrix \code{x}.  It returns the upper triangular
factor of the decomposition, i.e., the matrix \eqn{R} such that
\eqn{R'R = x} (see example).

Note that effectively, only the upper triangular part of \code{x} is
used such that the above only holds when \code{x} \emph{is} symmetric.
}
\references{Dongarra, J. J., J. R. Bunch, C. B. Moler and G. W. Stewart (1978).
\emph{LINPACK Users Guide}.  Philadelphia: SIAM Publications.
}
\seealso{\code{\link{chol2inv}} for its \emph{inverse},
  \code{\link{backsolve}} for solving linear systems with upper
  triangular left sides.

  \code{\link{qr}}, \code{\link{svd}} for related matrix factorizations.
}
\examples{
( m <- matrix(c(5,1,1,3),2,2) )
( cm <- chol(m) )
t(cm) \%*\% cm  #-- = 'm'
all(abs(m  -  t(cm) \%*\% cm) < 100* .Machine$double.eps) # TRUE
}
\keyword{algebra}
\keyword{array}
