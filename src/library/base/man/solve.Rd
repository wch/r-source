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
\seealso{
  \code{\link{backsolve}}, \code{\link{qr.solve}}.
}
\examples{
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
h8 <- hilbert(8); h8
\dontrun{solve(h8) # gives error: `singular'}
sh8 <- solve(h8, tol = 1e-10)
round(sh8 \%*\% h8, 3)
}
\keyword{algebra}
