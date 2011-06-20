% File src/library/stats/man/sd.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
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
  \item{x}{a numeric vector, matrix or data frame. An object which is
    not a vector, matrix or data frame is coerced (if possible)
    by \code{as.vector}.}
  \item{na.rm}{logical. Should missing values be removed?}
}
\details{
  Like \code{\link{var}} this uses denominator \eqn{n - 1}.
  
  The standard deviation of a zero-length vector (after removal of
  \code{NA}s if \code{na.rm = TRUE}) is not defined and gives an error.
  The standard deviation of a length-one vector is \code{NA}.
}
\seealso{
  \code{\link{var}} for its square, and \code{\link{mad}}, the most
  robust alternative.
}
\examples{
sd(1:2) ^ 2
}
\keyword{univar}
