\name{det}
\title{Calculate the Determinant of a Matrix}
\usage{
det(x, method = c("qr","eigenvalues"))
}
\description{
  \code{det} calculates the determinant of a matrix either by QR
  decomposition or from the eigenvalues, see \code{\link{qr}} and
  \code{\link{eigen}}.
  }
\arguments{
  \item{z}{numeric matrix.}
  \item{method}{\code{"qr"} (default) or \code{"eigenvalues"}}
}
\note{
  Often, computing the determinant is \emph{not} what you should be doing
  to solve a given problem.

  The \code{"qr"} method is much faster for large matrices.
}
\seealso{\code{\link{eigen}}, \code{\link{qr}}, \code{\link{svd}}}
\examples{
(x <- matrix(1:4, ncol=2))
det(x)
det(x, method="eigenvalues")

det(print(cbind(1,1:3,c(2,0,1))))
}
\keyword{array}
