% File src/library/base/man/chol2inv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2020 R Core Team
% Distributed under GPL 2 or later

\name{chol2inv}
\alias{chol2inv}
\title{Inverse from Cholesky (or QR) Decomposition}
\description{
  Invert a symmetric, positive definite square matrix from its Cholesky
  decomposition.  Equivalently, compute \eqn{(X'X)^{-1}}{(X'X)^(-1)}
  from the (\eqn{R} part) of the QR decomposition of \eqn{X}.
}
\usage{
chol2inv(x, size = NCOL(x), LINPACK = FALSE)
}
\arguments{
  \item{x}{a matrix.  The first \code{size} columns of the upper triangle
    contain the Cholesky decomposition of the matrix to be inverted.}
  \item{size}{the number of columns of \code{x} containing the
    Cholesky decomposition.}
  \item{LINPACK}{logical.  Defunct and gives an error.}

}
\value{
  The inverse of the matrix whose Cholesky decomposition was given.

  Unsuccessful results from the underlying LAPACK code will result in an
  error giving a positive error code: these can only be interpreted by
  detailed study of the FORTRAN code.
}
\source{
  This is an interface to the LAPACK routine \code{DPOTRI}.
  LAPACK is from \url{https://netlib.org/lapack/} and its guide is listed
  in the references.
}
\references{
  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition.
  SIAM.
  Available on-line at
  \url{https://netlib.org/lapack/lug/lapack_lug.html}.

  Dongarra, J. J., Bunch, J. R., Moler, C. B. and Stewart, G. W. (1978)
  \emph{LINPACK Users Guide}.
  Philadelphia: SIAM Publications.
}
\seealso{
  \code{\link{chol}}, \code{\link{solve}}.
}
\examples{
cma <- chol(ma  <- cbind(1, 1:3, c(1,3,7)))
ma \%*\% chol2inv(cma)
}
\keyword{algebra}
\keyword{array}
