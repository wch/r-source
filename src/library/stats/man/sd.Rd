% File src/library/stats/man/sd.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later

\name{sd}
\alias{sd}
\title{Standard Deviation}
\description{
  This function computes the standard deviation of the values in
  \code{x}.
  If \code{na.rm} is \code{TRUE} then missing values are removed before
  computation proceeds.
  If \code{x} is a matrix or a data frame, a vector of the standard
  deviation of the columns is returned.
}
\usage{
sd(x, na.rm = FALSE)
}
\arguments{
  \item{x}{a numeric vector, matrix or data frame.}
  \item{na.rm}{logical. Should missing values be removed?}
}
\seealso{
  \code{\link{var}} for its square, and \code{\link{mad}}, the most
  robust alternative.
}
\examples{
sd(1:2) ^ 2
}
\keyword{univar}
