\name{lower.tri}
\title{Lower and Upper Triangular Part of a Matrix}
\usage{
lower.tri(x, diag=FALSE)
upper.tri(x, diag=FALSE)
}
\alias{lower.tri}
\alias{upper.tri}
\arguments{
\item{x}{a matrix.}
\item{diag}{logical. Should the diagonal be included?}
}
\value{
  Returns a \bold{\code{\link{logical}}} matrix of the same dimension as
  \code{x} which is \code{TRUE} for the relevant triangular part of \code{x}.
}
\seealso{
\code{\link{diag}}, \code{\link{matrix}}.
}
\examples{
m2 <- ma <- matrix(1:20, 4, 5)
m2[lower.tri(m2)] <- NA
m2
all(lower.tri(ma) == !upper.tri(ma, diag=TRUE))
}
\keyword{array}
