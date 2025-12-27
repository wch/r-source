% File src/library/base/man/rev.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2025 R Core Team
% Distributed under GPL 2 or later

\name{rev}
\alias{rev}
\alias{rev.default}
\title{Reverse Elements}
\usage{
rev(x)
}
\arguments{
  \item{x}{a vector or another object for which reversal is defined.}
}
\description{
  \code{rev} provides a reversed version of its argument.  It is generic
  function with a default method for vectors and one for
  \code{\link{dendrogram}}s.

  To obtain a vector sorted in decreasing order efficiently, use
  \code{\link{sort}(x, decreasing = TRUE)}, not \code{rev(sort(x))}.
}
\references{
  \bibshow{R:Becker+Chambers+Wilks:1988}
}
\seealso{\code{\link{seq}}, \code{\link{sort}}.}
\examples{
x <- c(1:5, 5:3)
## sort into descending order; first more efficiently:
stopifnot(sort(x, decreasing = TRUE) == rev(sort(x)))
stopifnot(rev(1:7) == 7:1)  #- don't need 'rev' here
}
\keyword{manip}
