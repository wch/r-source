\name{grid}
\title{Add Grid to a Plot}
\usage{
grid(nx = 3, ny = 3, col = "lightgray", lty = "dotted")
}
\alias{grid}
\description{
  \code{grid} adds an \code{nx} by \code{ny} rectangular grid to an
  existing plot, using lines of type \code{lty} and color \code{col}.
}
\seealso{
  \code{\link{plot}}, \code{\link{lines}}, \code{\link{points}}.
}
\examples{
data(iris)
plot(iris$Sepal.L, iris$Sepal.W, col = rep(1:3, rep(50, 3)),
     xlim = c(4, 8), ylim = c(2, 4.5))
grid(4, 5, lty = "solid")
}
\keyword{aplot}
