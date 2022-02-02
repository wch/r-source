% File src/library/base/man/isSymmetric.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{isSymmetric}
\alias{isSymmetric}
\alias{isSymmetric.matrix}
\title{Test if a Matrix or other Object is Symmetric (Hermitian)}
\description{
  Generic function to test if \code{object} is symmetric or not.
  Currently only a matrix method is implemented, where a
  \code{\link{complex}} matrix \code{Z} must be \dQuote{Hermitian} for
  \code{isSymmetric(Z)} to be true.
}
\usage{
isSymmetric(object, \dots)
\method{isSymmetric}{matrix}(object, tol = 100 * .Machine$double.eps,
            tol1 = 8 * tol, \dots)
}
\arguments{
  \item{object}{any \R object; a \code{\link{matrix}} for the matrix method.}
  \item{tol}{numeric scalar >= 0.  Smaller differences are not
    considered, see \code{\link{all.equal.numeric}}.}
  \item{tol1}{numeric scalar >= 0.  \code{isSymmetric.matrix()}
    \sQuote{pre-tests} the first and last few rows for fast detection of
    \sQuote{obviously} asymmetric cases with this tolerance.  Setting it
    to length zero will skip the pre-tests.}
  \item{\dots}{further arguments passed to methods; the matrix method
    passes these to \code{\link{all.equal}}.  If the row and column
    names of \code{object} are allowed to differ for the symmetry check
    do use \code{check.attributes = FALSE}!}
}
\value{
  logical indicating if \code{object} is symmetric or not.
}
\details{
  The \code{\link{matrix}} method is used inside \code{\link{eigen}} by
  default to test symmetry of matrices \emph{up to rounding error}, using
  \code{\link{all.equal}}.  It might not be appropriate in all
  situations.

  Note that a matrix \code{m} is only symmetric if its \code{rownames} and
  \code{colnames} are identical.  Consider using \code{\link{unname}(m)}.
}
\seealso{\code{\link{eigen}} which calls \code{isSymmetric} when its
  \code{symmetric} argument is missing.
}
\examples{
isSymmetric(D3 <- diag(3)) # -> TRUE

D3[2, 1] <- 1e-100
D3
isSymmetric(D3) # TRUE
isSymmetric(D3, tol = 0) # FALSE for zero-tolerance

## Complex Matrices - Hermitian or not
Z <- sqrt(matrix(-1:2 + 0i, 2)); Z <- t(Conj(Z)) \%*\% Z
Z
isSymmetric(Z)      # TRUE
isSymmetric(Z + 1)  # TRUE
isSymmetric(Z + 1i) # FALSE -- a Hermitian matrix has a *real* diagonal

colnames(D3) <- c("X", "Y", "Z")
isSymmetric(D3)                         # FALSE (as row and column names differ)
isSymmetric(D3, check.attributes=FALSE) # TRUE  (as names are not checked)
}
\keyword{array}
\keyword{utilities}
