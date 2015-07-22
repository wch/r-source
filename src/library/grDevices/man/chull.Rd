% File src/library/grDevices/man/chull.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
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

  The algorithm is that given by Eddy (1977).
}
\value{
  An integer vector giving the indices of the unique points lying on the
  convex hull, in clockwise order.  (The first will be returned for
  duplicate points.)
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Eddy, W. F. (1977) A new convex hull algorithm for planar sets.
  \emph{ACM Transactions on Mathematical Software}, \bold{3}, 398--403.

  Eddy, W. F. (1977) Algorithm 523. CONVEX, A new convex hull
  algorithm for planar sets[Z]. \emph{ACM Transactions on
    Mathematical Software}, \bold{3}, 411--412.
}
\seealso{\code{\link{xy.coords}}, \code{\link{polygon}}}

\examples{
X <- matrix(stats::rnorm(2000), ncol = 2)
chull(X)
\dontrun{
  # Example usage from graphics package
  plot(X, cex = 0.5)
  hpts <- chull(X)
  hpts <- c(hpts, hpts[1])
  lines(X[hpts, ])
}
}
\keyword{graphs}
