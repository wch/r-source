\name{dendrogram}
\title{General Tree Structures}
\alias{as.dendrogram}
\alias{as.dendrogram.hclust}
\alias{plot.dendrogram}
\alias{plotNode}
\alias{plotNodeLimit}
\alias{cut.dendrogram}
\description{Class \code{"dendrogram"} provides general functions for
  handling tree-like structures. It is
  intended as a replacement for similar functions in hierarchical
  clustering and classification/regression trees, such that all of these
  can use the same engine for plotting or cutting trees. Currently the
  code is in alpha stage and the API may change at any point in time.}
\usage{
plot.dendrogram(dobj, type=c("rectangle", "triangle"),
                center=FALSE, xlab="", ylab="", ...)
as.dendrogram.hclust(object, ...)
cut.dendrogram(dobj, h)

plotNode(x1, x2, subtree, type, center)
plotNodeLimit(x1, x2, subtree, center)
}
\arguments{
  \item{dobj}{Object of class \code{"dendrogram"}.}
  \item{type}{Type of plot.}
  \item{center}{If \code{TRUE}, nodes are plotted centered with respect to
    the leaves in the branch. Default is to plot them in the middle of all
    direct child nodes.}
  \item{h}{Height at which the tree is cutted.}
}
\details{
  Warning: This documentation is preliminary.

  The dendrogram is directly represented as a nested list, where each list
  component corresponds to a branch of the tree. Hence, the first branch
  of tree \code{z} is \code{z[[1]]}, the second branch of the
  corresponding subtree is \code{z[[1]][[2]]} etc.. Each node of the tree
  carries some information needed for efficient plotting or cutting as
  attributes:
  \describe{
    \item{\code{members}}{number of leaves in the branch}
    \item{\code{height}}{Height at which the node is plotted}
    \item{\code{midpoint}}{Horizontal distance of the node from the left
      border of the branch}
    \item{\code{text}}{Text label of the node}
    \item{edgetext}{Text label for the edge leading to the node}
  }
  Terminal nodes (leaves of the tree) can have arbitrary value but must
  not be a list.

  \code{cut.dendrogram()} returns a list with components \code{$upper} and
  \code{$lower}, the first is a truncated version of the original tree,
  the latter a list with the branches obtained from cutting the tree.

  \code{plotNode()} and \code{plotNodeLimit()} are helper functions.
}
\examples{
library(mva)
data(USArrests)
hc <- hclust(dist(USArrests), "ave")
str(dend1 <- as.dendrogram(hc))
plot(dend1)
dend2 <- cut(dend1, h=70)
plot(dend2$upper)
plot(dend2$lower[[3]])
}
%FIXME !! -- proper Keyword !!
\keyword{multivariate}

