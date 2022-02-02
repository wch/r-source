% File src/library/stats/man/toeplitz.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{toeplitz}
\title{Form Symmetric Toeplitz Matrix}
\usage{
toeplitz(x)
}
\alias{toeplitz}
\arguments{
  \item{x}{the first row to form the Toeplitz matrix.}
}
\description{
  Forms a symmetric Toeplitz matrix given its first row.
}
\value{
  The Toeplitz matrix.
}
\author{A. Trapletti}
\examples{
x <- 1:5
toeplitz (x)
}
\keyword{ts}

