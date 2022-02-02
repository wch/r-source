% File src/library/stats/man/rect.hclust.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{rect.hclust}
\alias{rect.hclust}
\title{Draw Rectangles Around Hierarchical Clusters}
\description{
  Draws rectangles around the branches of a dendrogram highlighting the
  corresponding clusters. First the dendrogram is cut at a certain
  level, then a rectangle is drawn around selected branches.
}
\usage{
rect.hclust(tree, k = NULL, which = NULL, x = NULL, h = NULL,
            border = 2, cluster = NULL)
}
\arguments{
 \item{tree}{an object of the type produced by \code{hclust}.}
 \item{k, h}{Scalar. Cut the dendrogram such that either exactly
   \code{k} clusters are produced or by cutting at height \code{h}.}
 \item{which, x}{A vector selecting the clusters around which a
   rectangle should be drawn. \code{which} selects clusters by number
   (from left to right in the tree), \code{x} selects clusters
   containing the respective horizontal coordinates. Default is
   \code{which = 1:k}.}
 \item{border}{Vector with border colors for the rectangles.}
 \item{cluster}{Optional vector with cluster memberships as returned by
   \code{cutree(hclust.obj, k = k)}, can be specified for efficiency if
   already computed.}
}
\value{
  (Invisibly) returns a list where each element contains a vector of
  data points contained in the respective cluster.}
\seealso{
  \code{\link{hclust}}, \code{\link{identify.hclust}}.
}
\examples{
require(graphics)

hca <- hclust(dist(USArrests))
plot(hca)
rect.hclust(hca, k = 3, border = "red")
x <- rect.hclust(hca, h = 50, which = c(2,7), border = 3:4)
x
}
\keyword{aplot}
\keyword{cluster}
