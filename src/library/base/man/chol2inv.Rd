% File src/library/base/man/chol2inv.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2026 R Core Team
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
chol2inv(x, size = NCOL(x), LINPACK = FALSE, diag.only = FALSE)
}
\arguments{
  \item{x}{a matrix.  The first \code{size} columns of the upper triangle
    contain the Cholesky decomposition of the matrix to be inverted.}
  \item{size}{the number of columns of \code{x} containing the
    Cholesky decomposition.}
  \item{LINPACK}{logical.  Defunct and gives an error.}
  \item{diag.only}{logical; return only the diagonal of the inverse.}
}
\value{
  The inverse of the matrix whose Cholesky decomposition was given, or a
  numeric vector containing its diagonal if \code{diag.only = TRUE}.

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
  \bibshow{R:Anderson+Bai+Bischof:1999,
    R:Dongarra+Bunch+Moler:1979}
}
\seealso{
  \code{\link{chol}}, \code{\link{solve}}.
}
\examples{
cma <- chol(ma <- cbind(1, 1:3, c(1,3,7)))
ma \%*\% (chi <- chol2inv(cma)) #% = diag(3)
diag(chi) # you can get directly via
chol2inv(cma, diag.only = TRUE)

## 4x4 example:
tr <- rbind(c(1, 0,0,0),
            c(3,-1,0,0),
            c(0,1,-1,0), 4:1)
tr
(ma <- 5*diag(4)+ tr+ t(tr)) # symmetric (by construction)
cma <- chol(ma)
chima <- chol2inv(cma)
chima * 90
stopifnot(all.equal(diag(4), chima \%*\% ma, %
                    tolerance = 1e-14)) # seen 2.4e-15
## using size <= ncol(.):
str(Lchi <- lapply(1:4, \(k) chol2inv(cma, size = k)))
stopifnot(identical(Lchi[[4]], chima))
## equivalence (size = k):
for(k in 1:4) stopifnot(all.equal(Lchi[[k]], chol2inv(cma[1:k,1:k])))
## diag.only:
for(k in 1:4)
    stopifnot(all.equal(chol2inv(cma, size = k, diag.only=TRUE), diag(Lchi[[k]])))
}
\keyword{algebra}
\keyword{array}
