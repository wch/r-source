% file lqs/man/cov.rob.Rd
% copyright (C) 1998-9 B. D. Ripley
%
\name{cov.rob}
\alias{cov.rob}
\alias{cov.mve}
\alias{cov.mcd}
\title{
  Resistant Estimation of Multivariate Location and Scatter
}
\description{
  Compute a multivariate location and scale estimate with a high
  breakdown point -- this can be thought of as estimating the mean and
  covariance of the \code{good} part of the data. \code{cov.mve} and
  \code{cov.mcd} are compatibility wrappers.
}
\usage{
cov.rob(x, cor = FALSE, quantile.used = floor((n + p + 1)/2),
        method = c("mve", "mcd", "classical"), nsamp = "best", seed)

cov.mve(\dots)
cov.mcd(\dots)
}
\arguments{
  \item{x}{
    a matrix or data frame.
  }
  \item{cor}{
    should the returned result include a correlation matrix?
  }
  \item{quantile.used}{
    the minimum number of the data points regarded as \code{good} points.
  }
  \item{method}{
    the method to be used -- minimum volume ellipsoid, minimum
    covariance determinant or classical product-moment. Using
    \code{cov.mve} or \code{cov.mcd} forces \code{mve} or \code{mcd}
    respectively.
  }
  \item{nsamp}{
    the number of samples or \code{"best"} or \code{"exact"} or
    \code{"sample"}.
    If \code{"sample"} the number chosen is \code{min(5*p, 3000)}, taken
    from Rousseeuw and Hubert (1997). If \code{"best"} exhaustive
    enumeration is done up to 5000 samples: if \code{"exact"}
    exhaustive enumeration will be attempted however many samples are needed.
  }
  \item{seed}{
    the seed to be used for random sampling: see \code{\link{RNGkind}}. The
    current value of \code{.Random.seed} will be preserved if it is set.
  }
  \item{\dots}{arguments to \code{cov.rob} other than \code{method}.}
}
\value{
  A list with components

  \item{center}{
    the final estimate of location.
  }
  \item{cov}{
    the final estimate of scatter.
  }
  \item{cor}{
    (only is \code{cor = TRUE}) the estimate of the correlation
    matrix.
  }
  \item{sing}{
    message giving number of singular samples out of total
  }
  \item{crit}{
    the value of the criterion on log scale. For MCD this is
    the determinant, and for MVE it is proportional to the volume.
  }
  \item{best}{
    the subset used. For MVE the best sample, for MCD the best
    set of size \code{quantile.used}.
  }
  \item{n.obs}{
    total number of observations.
}}
\details{
  For method \code{"mve"}, an approximate search is made of a subset of
  size \code{quantile.used} with an enclosing ellipsoid of smallest volume; in
  method \code{"mcd"} it is the volume of the Gaussian confidence
  ellipsoid, equivalently the determinant of the classical covariance
  matrix, that is minimized. The mean of the subset provides a first
  estimate of the location, and the rescaled covariance matrix a first
  estimate of scatter. The Mahalanobis distances of all the points from
  the location estimate for this covariance matrix are calculated, and
  those points within the 97.5\% point under Gaussian assumptions are
  declared to be \code{good}. The final estimates are the mean and rescaled
  covariance of the \code{good} points.

  The rescaling is by the appropriate percentile under Gaussian data; in
  addition the first covariance matrix has an \emph{ad hoc} finite-sample
  correction given by Marazzi.

  For method \code{"mve"} the search is made over ellipsoids determined
  by the covariance matrix of \code{p} of the data points. For method
  \code{"mcd"} an additional improvement step suggested by Rousseeuw and
  van Driessen (1999) is used, in which once a subset of size
  \code{quantile.used} is selected, an ellipsoid based on its covariance
  is tested (as this will have no larger a determinant, and may be smaller).
}
\author{B.D. Ripley}
\references{
  P. J. Rousseeuw and A. M. Leroy (1987) 
  \emph{Robust Regression and Outlier Detection.}
  Wiley.

  A. Marazzi (1993) 
  \emph{Algorithms, Routines and S Functions for Robust Statistics.}
  Wadsworth and Brooks/Cole. 

  P. J. Rousseeuw and B. C. van Zomeren (1990) Unmasking
  multivariate outliers and leverage points, 
  \emph{Journal of the American Statistical Association}, \bold{85}, 633--639.

  P. J. Rousseeuw and K. van Driessen (1999) A fast algorithm for the
  minimum covariance determinant estimator. \emph{Technometrics}
  \bold{41}, 212--223.

  P. Rousseeuw and M. Hubert (1997) Recent developments in PROGRESS. In
  \emph{L1-Statistical Procedures and Related Topics }
  ed Y. Dodge, IMS Lecture Notes volume \bold{31}, pp. 201--214.
}
\seealso{
\code{\link{lqs}}
}
\examples{
data(stackloss)
set.seed(123)
cov.rob(stackloss)
cov.rob(stack.x, method = "mcd", nsamp = "exact")
}
\keyword{robust}
\keyword{multivariate}
