% File src/library/base/man/isSymmetric.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{isSymmetric}
\alias{isSymmetric}
\alias{isSymmetric.matrix}
\title{Test if a Matrix or other Object is Symmetric}
\description{
  Generic function to test if \code{object} is symmetric or not.
  Currently only a matrix method is implemented.
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
    \dQuote{pre-tests} the first and last few rows for fast detection of
    \dQuote{obviously} asymmetric cases with this tolerance.  Setting it
    to length zero will skip the pre-tests.}
  \item{\dots}{further arguments passed to methods; the matrix method
    passes these to \code{\link{all.equal}}.}
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
}
\keyword{array}
\keyword{utilities}
