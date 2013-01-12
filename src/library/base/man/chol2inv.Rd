% File src/library/base/man/chol2inv.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{chol2inv}
\alias{chol2inv}
\title{Inverse from Choleski (or QR) Decomposition}
\description{
  Invert a symmetric, positive definite square matrix from its Choleski
  decomposition.  Equivalently, compute \eqn{(X'X)^{-1}}{(X'X)^(-1)}
  from the (\eqn{R} part) of the QR decomposition of \eqn{X}.
}
\usage{
chol2inv(x, size = NCOL(x), LINPACK = FALSE)
}
\arguments{
  \item{x}{a matrix.  The first \code{size} columns of the upper triangle
    contain the Choleski decomposition of the matrix to be inverted.}
  \item{size}{the number of columns of \code{x} containing the
    Choleski decomposition.}
  \item{LINPACK}{logical.  Defunct and ignored (with a warning for true value).}

}
\value{
  The inverse of the matrix whose Choleski decomposition was given.
}
\source{
  This is an interface to the LAPACK routine \code{DPOTRI}.
  LAPACK is from \url{http://www.netlib.org/lapack} and its guide is listed
  in the references.
}
\references{
  Anderson. E. and ten others (1999)
  \emph{LAPACK Users' Guide}. Third Edition.
  SIAM.
  Available on-line at
  \url{http://www.netlib.org/lapack/lug/lapack_lug.html}.

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
