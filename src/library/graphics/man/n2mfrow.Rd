\name{n2mfrow}
\alias{n2mfrow}
\title{Compute Default mfrow From Number of Plots}
\description{
  Easy setup for plotting multiple figures (in a rectangular layout) on
  one page.  This computes a sensible default for
  \code{\link{par}(mfrow)}.
}
\usage{
n2mfrow(nr.plots)
}
\arguments{
  \item{nr.plots}{integer; the number of plot figures you'll want to draw.}
}
\value{
  A length two integer vector \code{nr, nc} giving the number of rows
  and columns, fulfilling \code{nr >= nc >= 1} and \code{nr * nc >= nr.plots}.
}
\author{Martin Maechler}
\seealso{\code{\link{par}}, \code{\link{layout}}.}
\examples{
n2mfrow(8) # 3 x 3

n <- 5 ; x <- seq(-2,2, len=51)
## suppose now that 'n' is not known {inside function}
op <- par(mfrow = n2mfrow(n))
for (j in 1:n)
   plot(x, x^j, main = substitute(x^ exp, list(exp = j)), type = "l",
   col = "blue")

sapply(1:10, n2mfrow)
}
\keyword{dplot}
\keyword{utilities}
