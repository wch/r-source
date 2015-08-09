% File src/library/base/man/mat.or.vec.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{mat.or.vec}
\alias{mat.or.vec}
\title{Create a Matrix or a Vector}
\usage{
mat.or.vec(nr, nc)
}
\arguments{
  \item{nr, nc}{numbers of rows and columns.}
}
\description{
  \code{mat.or.vec} creates an \code{nr} by \code{nc} zero matrix if
  \code{nc} is greater than 1, and a zero vector of length \code{nr} if
  \code{nc} equals 1.
}
\examples{
mat.or.vec(3, 1)
mat.or.vec(3, 2)
}
\keyword{array}
