\name{kmeans}
\alias{kmeans}
\alias{print.kmeans}
\title{
K-Means Clustering
}
\description{
  Perform k-means clustering on a data matrix.
}
\usage{
kmeans(x, centers, iter.max = 10, nstart = 1,
       algorithm = c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen"))
}
\arguments{
  \item{x}{A numeric matrix of data, or an object that can be coerced to
    such a matrix (such as a numeric vector or a data frame with all
    numeric columns).}
  
  \item{centers}{Either the number of clusters or a set of initial
    (distinct) cluster centres.  If a number, a random set of (distinct)
    rows in \code{x} is chosen as the initial centres.}

  \item{iter.max}{The maximum number of iterations allowed.}

  \item{nstart}{If \code{centers} is a number, how many random sets
    should be chosen?}

  \item{algorithm}{character: may be abbreviated.}
}
\details{
  The data given by \code{x} is clustered by the \eqn{k}-means method,
  which aims to partition the points into \eqn{k} groups such that the
  sum of squares from points to the assigned cluster centres is minimized.
  At the minimum, all cluster centres are at the mean of their Voronoi
  sets (the set of data points which are nearest to the cluster centre).
 
  The algorithm of Hartigan and Wong (1979) is used by default.  Note
  that some authors use \eqn{k}-means to refer to a specific algorithm
  rather than the general method: most commonly the algorithm given by
  MacQueen (1967) but sometimes that given by Lloyd (1957) and Forgy
  (1965). The Hartigan--Wong algorithm generally does a better job than
  either of those, but trying several random starts is often
  recommended.

  Except for the Lloyd--Forgy method, \eqn{k} clusters will always be
  returned if a number is specified.
  If an initial matrix of centres is supplied, it is possible that
  no point will be closest to one or more centres, which is currently
  an error for the Hartigan--Wong method.
}
\value{
  An object of class \code{"kmeans"} which is a list with components:

  \item{cluster}{
    A vector of integers indicating the cluster to which each point is
    allocated.
  }
  \item{centers}{A matrix of cluster centres.}
  \item{withinss}{The within-cluster sum of squares for each cluster.}
  \item{size}{The number of points in each cluster.}

  There is a \code{print} method for this class.
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
  eds L. M. Le Cam \& J. Neyman,
  \bold{1}, pp. 281--297. Berkeley, CA: University of California Press.
}
\examples{
# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(cl <- kmeans(x, 2))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex=2)

## random starts do help here with too many clusters
(cl <- kmeans(x, 5, nstart = 25))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:5, pch = 8)
}
\keyword{multivariate}
\keyword{cluster}
