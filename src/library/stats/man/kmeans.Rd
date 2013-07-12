% File src/library/stats/man/kmeans.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{kmeans}
\alias{kmeans}
\alias{print.kmeans}
\alias{fitted.kmeans}
\title{
K-Means Clustering
}
\description{
  Perform k-means clustering on a data matrix.
}
\usage{
kmeans(x, centers, iter.max = 10, nstart = 1,
       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy",
                     "MacQueen"), trace=FALSE)
\method{fitted}{kmeans}(object, method = c("centers", "classes"), ...)
}
\arguments{
  \item{x}{numeric matrix of data, or an object that can be coerced to
    such a matrix (such as a numeric vector or a data frame with all
    numeric columns).}
  \item{centers}{either the number of clusters, say \eqn{k}, or a set of
    initial (distinct) cluster centres.  If a number, a random set of
    (distinct) rows in \code{x} is chosen as the initial centres.}
  \item{iter.max}{the maximum number of iterations allowed.}
  \item{nstart}{if \code{centers} is a number, how many random sets
    should be chosen?}
  \item{algorithm}{character: may be abbreviated.  Note that
    \code{"Lloyd"} and \code{"Forgy"} and alternative names for one
    algorithm.}
  \item{object}{an \R object of class \code{"kmeans"}, typically the
    result \code{ob} of \code{ob <- kmeans(..)}.}
  \item{method}{character: may be abbreviated. \code{"centers"} causes
    \code{fitted} to return cluster centers (one for each input point) and
    \code{"classes"} causes \code{fitted} to return a vector of class
    assignments.}
  \item{trace}{logical or integer number, currently only used in the
    default method (\code{"Hartigan-Wong"}): if positive (or true),
    tracing information on the progress of the algorithm is
    produced.  Higher values may produce more tracing information.}
  \item{\dots}{not used.}
}
\details{
  The data given by \code{x} are clustered by the \eqn{k}-means method,
  which aims to partition the points into \eqn{k} groups such that the
  sum of squares from points to the assigned cluster centres is minimized.
  At the minimum, all cluster centres are at the mean of their Voronoi
  sets (the set of data points which are nearest to the cluster centre).

  The algorithm of Hartigan and Wong (1979) is used by default.  Note
  that some authors use \eqn{k}-means to refer to a specific algorithm
  rather than the general method: most commonly the algorithm given by
  MacQueen (1967) but sometimes that given by Lloyd (1957) and Forgy
  (1965).  The Hartigan--Wong algorithm generally does a better job than
  either of those, but trying several random starts (\code{nstart}\eqn{>
  1}) is often recommended.  In rare cases, when some of the points
  (rows of \code{x}) are extremely close, the algorithm may not converge
  in the \dQuote{Quick-Transfer} stage, signalling a warning (and
  returning \code{ifault = 4}).  Slight
  rounding of the data may be advisable in that case.

  For ease of programmatic exploration, \eqn{k=1} is allowed, notably
  returning the center and \code{withinss}.

  Except for the Lloyd--Forgy method, \eqn{k} clusters will always be
  returned if a number is specified.
  If an initial matrix of centres is supplied, it is possible that
  no point will be closest to one or more centres, which is currently
  an error for the Hartigan--Wong method.
}
\value{
  \code{kmeans} returns an object of class \code{"kmeans"} which has a
  \code{print} and a \code{fitted} method.  It is a list with at least
  the following components:
  \item{cluster}{
    A vector of integers (from \code{1:k}) indicating the cluster to
    which each point is allocated.
  }
  \item{centers}{A matrix of cluster centres.}
  \item{totss}{The total sum of squares.}
  \item{withinss}{Vector of within-cluster sum of squares,
    one component per cluster.}
  \item{tot.withinss}{Total within-cluster sum of squares,
    i.e., \code{sum(withinss)}.}
  \item{betweenss}{The between-cluster sum of squares,
    i.e. \code{totss-tot.withinss}.}
  \item{size}{The number of points in each cluster.}
  \item{iter}{The number of (outer) iterations.}
  \item{ifault}{integer: indicator of a possible algorithm problem
    -- for experts.}
}
\references{
  Forgy, E. W. (1965) Cluster analysis of multivariate data:
  efficiency vs interpretability of classifications.
  \emph{Biometrics} \bold{21}, 768--769.

  Hartigan, J. A. and Wong, M. A. (1979).
  A K-means clustering algorithm.
  \emph{Applied Statistics} \bold{28}, 100--108.

  Lloyd, S. P. (1957, 1982)  Least squares quantization in PCM.
  Technical Note, Bell Laboratories.  Published in 1982 in
  \emph{IEEE Transactions on Information Theory} \bold{28}, 128--137.

  MacQueen, J. (1967)  Some methods for classification and analysis of
  multivariate observations. In \emph{Proceedings of the Fifth Berkeley
    Symposium on  Mathematical Statistics and  Probability},
  eds L. M. Le Cam & J. Neyman,
  \bold{1}, pp. 281--297. Berkeley, CA: University of California Press.
}
\examples{
require(graphics)

# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 2))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)

# sum of squares
ss <- function(x) sum(scale(x, scale = FALSE)^2)

## cluster centers "fitted" to each obs.:
fitted.x <- fitted(cl);  head(fitted.x)
resid.x <- x - fitted(cl)

## Equalities : ----------------------------------
cbind(cl[c("betweenss", "tot.withinss", "totss")], # the same two columns
         c(ss(fitted.x), ss(resid.x),    ss(x)))
stopifnot(all.equal(cl$ totss,        ss(x)),
	  all.equal(cl$ tot.withinss, ss(resid.x)),
	  ## these three are the same:
	  all.equal(cl$ betweenss,    ss(fitted.x)),
	  all.equal(cl$ betweenss, cl$totss - cl$tot.withinss),
	  ## and hence also
	  all.equal(ss(x), ss(fitted.x) + ss(resid.x))
	  )

kmeans(x,1)$withinss # trivial one-cluster, (its W.SS == ss(x))

## random starts do help here with too many clusters
## (and are often recommended anyway!):
(cl <- kmeans(x, 5, nstart = 25))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)
}
\keyword{multivariate}
\keyword{cluster}
