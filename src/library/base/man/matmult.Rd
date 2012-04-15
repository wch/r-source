% File src/library/base/man/matmult.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Team
% Distributed under GPL 2 or later

\name{matmult}
\alias{\%*\%}
\alias{matmult}
\title{Matrix Multiplication}
\description{
  Multiplies two matrices, if they are conformable.
  If one argument is a vector, it will be promoted to either a row or
  column matrix to make the two arguments conformable.
  If both are vectors it will return the inner product (as a matrix).
}
\usage{
x \%*\% y
}
\arguments{
  \item{x, y}{numeric or complex matrices or vectors.}
}
\details{
  When a vector is promoted to a matrix, its names are not
  promoted to row or column names, unlike \code{\link{as.matrix}}.

  This operator is S4 generic but not S3 generic.  S4 methods need to be
  written for a function of two arguments named \code{x} and \code{y}.
}
\value{
  A double or complex matrix product.  Use \code{\link{drop}} to remove
  dimensions which have only one level.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\seealso{
  \code{\link{matrix}},
  \code{\link{Arithmetic}},
  \code{\link{diag}}.
}
\examples{
x <- 1:4
(z <- x \%*\% x)    # scalar ("inner") product (1 x 1 matrix)
drop(z)             # as scalar

y <- diag(x)
z <- matrix(1:12, ncol = 3, nrow = 4)
y \%*\% z
y \%*\% x
x \%*\% z
}
\keyword{array}
\keyword{arith}
