\name{hclust}
\title{Hierarchical Clustering}
\alias{hclust}
\alias{plot.hclust}
\alias{plclust}
\alias{print.hclust}
\description{
  Hierarchical cluster analysis on a set of dissimilarities and
  methods for analyzing it.
}
\usage{
hclust(d, method = "complete", members=NULL)

\method{plot}{hclust}(x, labels = NULL, hang = 0.1,
     axes = TRUE, frame.plot = FALSE, ann = TRUE,
     main = "Cluster Dendrogram",
     sub = NULL, xlab = NULL, ylab = "Height", \dots)

plclust(tree, hang = 0.1, unit = FALSE, level = FALSE, hmin = 0,
        square = TRUE, labels = NULL, plot. = TRUE,
        axes = TRUE, frame.plot = FALSE, ann = TRUE,
        main = "", sub = NULL, xlab = NULL, ylab = "Height")
}
\arguments{
  \item{d}{a dissimilarity structure as produced by \code{dist}.}

  \item{method}{the agglomeration method to be used. This should
    be (an unambiguous abbreviation of) one of
    \code{"ward"}, \code{"single"}, \code{"complete"},
    \code{"average"}, \code{"mcquitty"}, \code{"median"} or
    \code{"centroid"}.}

  \item{members}{\code{NULL} or a vector with length size of
    \code{d}. See the Details section.}

  \item{x,tree}{an object of the type produced by \code{hclust}.}

  \item{hang}{The fraction of the plot height by which labels should hang
    below the rest of the plot.
    A negative value will cause the labels to hang down from 0.}

  \item{labels}{A character vector of labels for the leaves of the
    tree. By default the row names or row numbers of the original data are
    used. If \code{labels=FALSE} no labels at all are plotted.}

  \item{axes, frame.plot, ann}{logical flags as in \code{\link{plot.default}}.}
  \item{main, sub, xlab, ylab}{character strings for
    \code{\link{title}}.  \code{sub} and \code{xlab} have a non-NULL
    default when there's a \code{tree$call}.}
  \item{\dots}{Further graphical arguments.}

  \item{unit}{logical.  If true, the splits are plotted at
    equally-spaced heights rather than at the height in the object.}

  \item{hmin}{numeric.  All heights less than \code{hmin} are regarded
    as being \code{hmin}: this can be used to suppress detail at the
    bottom of the tree.}
  \item{level, square, plot.}{as yet unimplemented arguments of
    \code{plclust} for S-PLUS compatibility.}
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

  \item{call}{the call which produced the result.}

  \item{method}{the cluster method that has been used.}

  \item{dist.method}{the distance that has been used to create \code{d}
    (only returned if the distance object has a \code{"method"}
    attribute).}

  There are \code{\link{print}}, \code{\link{plot}} and \code{identify}
  (see \code{\link{identify.hclust}}) methods and the
  \code{\link{rect.hclust}()} function for \code{hclust} objects.
  The \code{plclust()} function is basically the same as the plot method,
  \code{plot.hclust}, primarily for back compatibility with S-plus. Its
  extra arguments are not yet implemented.
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

  A number of different clustering methods are provided.  \emph{Ward's}
  minimum variance method aims at finding compact, spherical clusters.
  The \emph{complete linkage} method finds similar clusters. The
  \emph{single linkage} method (which is closely related to the minimal
  spanning tree) adopts a \sQuote{friends of friends} clustering
  strategy.  The other methods can be regarded as aiming for clusters
  with characteristics somewhere between the single and complete link
  methods.  Note however, that methods \code{"median"} and
  \code{"centroid"} are \emph{not} leading to a \emph{monotone distance}
  measure, or equivalently the resulting dendrograms can have so called
  \emph{inversions} (which are hard to interpret).

  If \code{members!=NULL}, then \code{d} is taken to be a
  dissimilarity matrix between clusters instead of dissimilarities
  between singletons and \code{members} gives the number of observations
  per cluster.  This way the hierarchical cluster algorithm can be
  \dQuote{started in the middle of the dendrogram}, e.g., in order to
  reconstruct the part of the tree above a cut (see examples).
  Dissimilarities between clusters can be efficiently computed (i.e.,
  without \code{hclust} itself) only for a limited number of
  distance/linkage combinations, the simplest one being squared
  Euclidean distance and centroid linkage.  In this case the
  dissimilarities between the clusters are the squared Euclidean
  distances between cluster means.

  In hierarchical cluster displays, a decision is needed at each merge to
  specify which subtree should go on the left and which on the right.
  Since, for \eqn{n} observations there are \eqn{n-1} merges,
  there are \eqn{2^{(n-1)}} possible orderings for the leaves
  in a cluster tree, or dendrogram.
  The algorithm used in \code{hclust} is to order the subtree so that
  the tighter cluster is on the left (the last, i.e., most recent,
  merge of the left subtree is at a lower value than the last
  merge of the right subtree).
  Single observations are the tightest clusters possible,
  and merges involving two observations place them in order by their
  observation sequence number.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole. (S version.)

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

  Gordon, A. D. (1999).
  \emph{Classification}. Second Edition.
  London: Chapman and Hall / CRC

  Murtagh, F. (1985).
  \dQuote{Multidimensional Clustering Algorithms}, in
  \emph{COMPSTAT Lectures 4}.
  Wuerzburg: Physica-Verlag
  (for algorithmic details of algorithms used).

  McQuitty, L.L. (1966).
  Similarity Analysis by Reciprocal Pairs for Discrete and Continuous
  Data.
  \emph{Educational and Psychological Measurement}, \bold{26}, 825--831.
}
\author{
  The \code{hclust} function is based on Fortran code
  contributed to STATLIB by F. Murtagh.
}
\seealso{
  \code{\link{identify.hclust}}, \code{\link{rect.hclust}},
  \code{\link{cutree}}, \code{\link{dendrogram}}, \code{\link{kmeans}}.

  For the Lance--Williams formula and methods that apply it generally,
  see \code{\link[cluster]{agnes}} from package \pkg{cluster}.
}
\examples{
hc <- hclust(dist(USArrests), "ave")
plot(hc)
plot(hc, hang = -1)

## Do the same with centroid clustering and squared Euclidean distance,
## cut the tree into ten clusters and reconstruct the upper part of the
## tree from the cluster centers.
hc <- hclust(dist(USArrests)^2, "cen")
memb <- cutree(hc, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)
}
\keyword{multivariate}
\keyword{cluster}
