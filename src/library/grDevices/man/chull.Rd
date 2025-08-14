% File src/library/grDevices/man/chull.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2024 R Core Team
% Distributed under GPL 2 or later

\name{chull}
\alias{chull}
\title{Compute Convex Hull of a Set of Points}
\usage{
chull(x, y = NULL)
}
\arguments{
  \item{x, y}{coordinate vectors of points. This can be specified as two
    vectors \code{x} and \code{y}, a 2-column matrix \code{x}, a list
    \code{x} with two components, etc, see \code{\link{xy.coords}}.}
}
\description{
  Computes the subset of points which lie on the convex hull of the
  set of points specified.
}
\details{
  \code{\link{xy.coords}} is used to interpret the specification of the
  points. Infinite, missing and \code{NaN} values are not allowed.

  The algorithm is that given by \bibcitet{R:Eddy:1977a, R:Eddy:1977b}.
}
\value{
  An integer vector giving the indices of the unique points lying on the
  convex hull, in clockwise order.  (The first will be returned for
  duplicate points.)
}
\references{
  \bibshow{*, R:Becker+Chambers+Wilks:1988}
}
\seealso{
  \code{\link{xy.coords}},
  \code{\link{polygon}}
}
\examples{
X <- matrix(stats::rnorm(2000), ncol = 2)
chull(X)

plot(X, cex = 0.5)
polygon(X[chull(X), ])
}
\keyword{graphs}
