\name{solve}
\title{Solve a System of Equations}
\usage{
\method{solve}{default}(a, b, tol = 1e-7)
}
\alias{solve}
\alias{solve.default}
\alias{solve.qr}
\arguments{
  \item{a}{a numeric matrix containing the coefficients of the linear
    system.}
  \item{b}{a numeric vector or matrix giving the right-hand side(s) of
    the linear system.  If omitted, \code{b} is taken to be an identity
    matrix and \code{solve} will return the inverse of \code{a}.}
  \item{tol}{the tolerance for detecting linear dependencies in the
    columns of \code{a}.}
}
\description{
  This generic function solves the equation \code{a \%*\% x = b} for \code{x},
  where \code{b} can be either a vector or a matrix.
}
\details{
  As from \R version 1.3.0, \code{a} or \code{b} can be complex, in
  which case LAPACK routine \code{ZESV} is used.
  This uses double complex arithmetic which might not beavailable on all
  platforms.
}
\seealso{
  \code{\link{backsolve}}, \code{\link{qr.solve}}.
}
\examples{
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8); h8
\dontrun{solve(h8) # gives error: `singular'}
sh8 <- solve(h8, tol = 1e-10)
round(sh8 \%*\% h8, 3)

A <- hilbert(4)
A[] <- as.complex(A)
## might not be supported on all platforms
try(solve(A))
}
\keyword{algebra}
