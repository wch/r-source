\name{grid}
\title{Add Grid to a Plot}
\usage{
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
}
\arguments{
  \item{nx,ny}{number of cells of the grid in x and y direction.
    Defaults to the number of tick marks on the corresponding axis.}
  \item{col}{character or (integer) numeric; color of the grid lines.}
  \item{lty}{character or (integer) numeric; line type of the grid lines.}
}
\alias{grid}
\description{
  \code{grid} adds an \code{nx} by \code{ny} rectangular grid to an
  existing plot, using lines of type \code{lty} and color \code{col}.

  If more fine tuning is required, use \code{\link{abline}(h = ., v = .)}
  directly.
}
\seealso{
  \code{\link{plot}}, \code{\link{abline}}, \code{\link{lines}},
  \code{\link{points}}.
}
\examples{
data(iris)
## maybe change the desired number of tick marks:  par(lab=c(mx,my,7))
plot(iris$Sepal.L, iris$Sepal.W, col = rep(1:3, rep(50, 3)),
     xlim = c(4, 8), ylim = c(2, 4.5), panel.first = grid())
}
\keyword{aplot}
