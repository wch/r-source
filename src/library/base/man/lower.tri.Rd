\name{lower.tri}
\alias{lower.tri}
\alias{upper.tri}
\title{Lower and Upper Triangular Part of a Matrix}
\description{
  Returns a matrix of logicals the same size of a given matrix with
  entries \code{TRUE} in the lower or upper triangle.
}
\usage{
lower.tri(x, diag = FALSE)
upper.tri(x, diag = FALSE)
}
\arguments{
  \item{x}{a matrix.}
  \item{diag}{logical. Should the diagonal be included?}
}
\seealso{
  \code{\link{diag}},
  \code{\link{matrix}}.
}
\examples{
(m2 <- matrix(1:20, 4, 5))
lower.tri(m2)
m2[lower.tri(m2)] <- NA
m2
}
\keyword{array}
