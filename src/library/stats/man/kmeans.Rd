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
kmeans(x, centers, iter.max = 10)
}
\arguments{
  \item{x}{A numeric matrix of data, or an object that can be coerced to
    such a matrix (such as a numeric vector or a data frame with all
    numeric columns).}
  \item{centers}{Either the number of clusters or a set of initial
    cluster centres.  If the first, a random set of (distinct) rows
    in \code{x} is chosen as the initial centres.}
  \item{iter.max}{The maximum number of iterations allowed.}
}
\details{
  The data given by \code{x} is clustered by the \eqn{k}-means method,
  which aims to partition the points into \eqn{k} groups such that the
  sum of squares from points to the assigned cluster centres is minimized.
  At the minimum, all cluster centres are at the mean of their Voronoi
  sets (the set of data points which are nearest to the cluster centre).
 
  The algorithm of Hartigan and Wong (1979) is used.  Note that some
  authors use \eqn{k}-means to refer to a specific algorithm rather than
  the general method: most commonly the algorithm given by MacQueen but
  sometimes that given by Lloyd and later by Forgy.  The Hartigan--Wong
  algorithm generally does a better job than either of those, but trying
  several random starts is often recommended.

  There is a \code{print} method.
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
}
\references{
  Hartigan, J. A. and Wong, M. A. (1979).
  A K-means clustering algorithm.
  \emph{Applied Statistics} \bold{28}, 100--108.
}
\examples{
# a 2-dimensional example
x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
(cl <- kmeans(x, 2, 20))
plot(x, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8)
}
\keyword{multivariate}
\keyword{cluster}
