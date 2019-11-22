% File src/library/grDevices/man/n2mfrow.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{n2mfrow}
\alias{n2mfrow}
\title{Compute Default 'mfrow' From Number of Plots}
\description{
  Easy setup for plotting multiple figures (in a rectangular layout) on
  one page.  This computes a sensible default for
  \code{\link{par}(mfrow)}.
}
\usage{
n2mfrow(nr.plots, asp = 1)
}
\arguments{
  \item{nr.plots}{integer; the number of plot figures you'll want to draw.}
  \item{asp}{positive number; the target aspect ratio (columns / rows) in
    the output.  Was implicitly hardwired to \code{1}; because of that and
    back compatibility, there is a somewhat discontinuous behavior when
    varying \code{asp} around 1, for \code{nr.plots <= 12}.}
}
\value{
  A length-two integer vector \code{(nr, nc)} giving the positive number of rows
  and columns, fulfilling \code{nr * nc >= nr.plots}, and currently, for
  \code{asp = 1}, \code{nr >= nc >= 1}.
}
\note{
  Conceptually, this is a quadratic integer optimization problem, with
  inequality constraints \eqn{nr >= 1}, \eqn{nc >= 1}, and
  \eqn{nr.plots >= nr*nc} (and possibly \code{nr >= asp*nc}),
  and \emph{two} objective functions which would have to be combined via a
  tuning weight, say \eqn{w}, to, e.g.,
  \eqn{(nr.plots - nr*nc)  + w |nr/nc - asp|}.

  The current algorithm is simple and not trying to solve one of these
  optimization problems.
}
\author{Martin Maechler; suggestion of \code{asp} by Michael Chirico.}
\seealso{\code{\link{par}}, \code{\link{layout}}.}
\examples{
require(graphics)

n2mfrow(8) # 3 x 3

n <- 5 ; x <- seq(-2, 2, len = 51)
## suppose now that 'n' is not known {inside function}
op <- par(mfrow = n2mfrow(n))
for (j in 1:n)
   plot(x, x^j, main = substitute(x^ exp, list(exp = j)), type = "l",
   col = "blue")

sapply(1:14, n2mfrow)
sapply(1:14, n2mfrow, asp=16/9)
}
\keyword{dplot}
\keyword{utilities}
