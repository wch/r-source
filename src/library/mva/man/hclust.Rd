\name{hclust}
\title{Hierarchical Clustering}
\alias{hclust}
\alias{plot.hclust}
\description{
  Performs a hierarchical cluster analysis on a set of dissimilarities.
}
\usage{
hclust(d, method = "complete")

plot.hclust(hclust.obj, labels, hang = 0.1, \dots)
}
\arguments{
  \item{d}{a dissimilarity structure as produced by \code{dist}.}

  \item{method}{the agglomeration method to be used. This should
    be (an unambiguous abbreviation of) one of
    \code{"ward"}, \code{"single"}, \code{"complete"},
    \code{"average"}, \code{"mcquitty"}, \code{"median"} or
    \code{"centroid"}.}

  \item{hclust.obj}{an object of the type produced by \code{hclust}.}

  \item{hang}{The fraction of the plot height which labels should hang
    below the rest of the plot.
    A negative value will cause the labels to hang down from 0.}

  \item{labels}{A character vector of of labels for the leaves of the
    tree. By default the row names or row numbers of the original data are
    used. If \code{labels=FALSE} no labels at all are plotted.}
}
\value{
  An object of class \bold{hclust} which describes the
  tree produced by the clustering process.
  The object is a list with components:

  \item{merge}{an \eqn{n-1} by 2 matrix.
    Row \eqn{i} of \code{merge} describes the merging of clusters
    at step \eqn{i} of the clustering.
    If an element \eqn{j} in the row is negative,
    then observation \eqn{-j} was merged at this stage.
    If \eqn{j} is positive then the merge
    was with the cluster formed at the (earlier) stage \eqn{j}
    of the algorithm.
    Thus negative entries in \code{merge} indicate agglomerations
    of singletons, and positive entries indicate agglomerations
    of non-singletons.}

  \item{height}{a set of \eqn{n-1} non-decreasing real values.
    The clustering \emph{height}: that is, the value of
    the criterion associated with the clustering
    \code{method} for the particular agglomeration.}

  \item{order}{a vector giving the permutation of the original
    observations suitable for plotting, in the sense that a cluster
    plot using this ordering and matrix \code{merge} will not have
    crossings of the branches.}

  \item{labels}{labels for each of the objects being clustered.}
}
\details{
  This function performs a hierarchical cluster analysis
  using a set of dissimilarities for the \eqn{n} objects being
  clustered.  Initially, each object is assigned to its own
  cluster and then the algorithm proceeds iteratively,
  at each stage joining the two most similar clusters,
  continuing until there is just a single cluster.
  At each stage distances between clusters are recomputed
  by the Lance--Williams dissimilarity update formula
  according to the particular clustering method being used.

  An number of different clustering methods are provided.
  \emph{Ward's} minimum variance method aims at finding compact,
  spherical clusters.  The \emph{complete linkage} method finds
  similar clusters. The \emph{single linkage} method
  (which is closely related to the minimal spanning tree)
  adopts a `friends of friends' clustering strategy.
  The other methods can be regarded as aiming
  for clusters with characteristics somewhere between
  the single and complete link methods.

  In hierarchical cluster displays, a decision is needed at each merge to
  specify which subtree should go on the left and which on the right.
  Since, for \eqn{n} observations there are \eqn{n-1} merges,
  there are \eqn{2^{(n-1)}} possible orderings for the leaves
  in a cluster tree, or dendrogram.
  The algorithm used in \code{hclust} is to order the subtree so that
  the tighter cluster is on the left (the last, i.e. most recent,
  merge of the left subtree is at a lower value than the last
  merge of the right subtree).
  Single observations are the tightest clusters possible,
  and merges involving two observations place them in order by their
  observation sequence number.
}
\references{
  Everitt, B. (1974).
  \emph{Cluster Analysis}.
  London: Heinemann Educ. Books.

  Hartigan, J. A. (1975).
  \emph{Clustering  Algorithms}.
  New York: Wiley.

  Sneath, P. H. A. and R. R. Sokal (1973).
  \emph{Numerical Taxonomy}.
  San Francisco: Freeman.

  Anderberg, M. R. (1973).
  \emph{Cluster Analysis for Applications}.
  Academic Press: New York.

  Gordon, A. D. (1981).
  \emph{Classification}.
  London: Chapman and Hall.

  Murtagh, F. (1985).
  ``Multidimensional Clustering Algorithms'', in
  \emph{COMPSTAT Lectures 4}.
  Wuerzburg: Physica-Verlag
  (for algorithmic details of algorithms used).
}
\author{
  The \code{hclust} function is based on Fortran code
  contributed to STATLIB by F. Murtagh.
}
\seealso{
  \code{\link{kmeans}}.
}
\examples{
library(mva)
data(USArrests)
hc <- hclust(dist(USArrests), "ave")
plot(hc, hang=-1)
plot(hc)
}
\keyword{multivariate}
\keyword{cluster}
