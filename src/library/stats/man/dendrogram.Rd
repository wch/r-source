\name{dendrogram}
\title{General Tree Structures}
\alias{dendrogram}% the class
\alias{as.dendrogram}
\alias{as.dendrogram.hclust}
\alias{cut.dendrogram}
\alias{[[.dendrogram}
\alias{print.dendrogram}
\alias{rev.dendrogram}
\alias{str.dendrogram}
\alias{plot.dendrogram}
%\alias{plotNode}
%\alias{plotNodeLimit}
\alias{is.leaf}
\description{
  Class \code{"dendrogram"} provides general functions for handling
  tree-like structures.  It is intended as a replacement for similar
  functions in hierarchical clustering and classification/regression
  trees, such that all of these can use the same engine for plotting or
  cutting trees.

  The code is still in testing stage and the API may
  change in the future.
}
\usage{
as.dendrogram(object, \dots)
\method{as.dendrogram}{hclust}(object, hang = -1, \dots)

\method{plot}{dendrogram}(x, type = c("rectangle", "triangle"),
      center = FALSE,
      edge.root = is.leaf(x) || !is.null(attr(x,"edgetext")),
      nodePar = NULL, edgePar = list(),
      leaflab = c("perpendicular", "textlike", "none"),
      dLeaf = NULL, xlab = "", ylab = "", xaxt = "n", yaxt = "s",
      horiz = FALSE, frame.plot = FALSE, \dots)

\method{cut}{dendrogram}(x, h, \dots)

\method{print}{dendrogram}(x, digits, \dots)

\method{rev}{dendrogram}(x)

\method{str}{dendrogram}(object, max.level = 0, digits.d = 3,
    give.attr = FALSE, wid = getOption("width"),
    nest.lev = 0, indent.str = "", stem = "--", \dots)

is.leaf(object)
}
\arguments{
  \item{object}{any \R object that can be made into one of class
    \code{"dendrogram"}.}
  \item{x}{object of class \code{"dendrogram"}.}
  \item{hang}{numeric scalar indicating how the \emph{height} of leaves
    should be computed from the heights of their parents; see
    \code{\link{plot.hclust}}.}
  \item{type}{type of plot.}
  \item{center}{logical; if \code{TRUE}, nodes are plotted centered with
    respect to the leaves in the branch.  Otherwise (default), plot them
    in the middle of all direct child nodes.}
  \item{edge.root}{logical; if true, draw an edge to the root node.}
  \item{nodePar}{a \code{list} of plotting parameters to use for the
    nodes (see \code{\link{points}}) or \code{NULL} by default which
    does not draw symbols at the nodes.  The list may contain components
    named \code{pch}, \code{cex}, \code{col}, and/or \code{bg} each of
    which can have length two for specifying separate attributes for
    \emph{inner} nodes and \emph{leaves}.}
  \item{edgePar}{a \code{list} of plotting parameters to use for the
    edge \code{\link{segments}} and labels (if there's an
    \code{edgetext}).  The list may contain components
    named \code{col}, \code{lty} and \code{lwd} (for the segments),
    \code{p.col}, \code{p.lwd}, and \code{p.lty} (for the
    \code{\link{polygon}} around the text) and \code{t.col} for the text
    color.  As with \code{nodePar}, each can have length two for
    differentiating leaves and inner nodes.
  }
  \item{leaflab}{a string specifying how leaves are labeled.  The
    default \code{"perpendicular"} write text vertically (by default).\cr
    \code{"textlike"} writes text horizontally (in a rectangle), and \cr
    \code{"none"} suppresses leaf labels.}
  \item{dLeaf}{a number specifying the \bold{d}istance in user
    coordinates between the tip of a leaf and its label.  If \code{NULL}
    as per default, 3/4 of a letter width or height is used.}
  \item{horiz}{logical indicating if the dendrogram should be drawn
    \emph{horizontally} or not.}
  \item{frame.plot}{logical indicating if a box around the plot should
    be drawn, see \code{\link{plot.default}}.}
  \item{h}{height at which the tree is cut.}
  \item{\dots, xlab, ylab, xaxt, yaxt}{graphical parameters, or arguments for
    other methods.}
  \item{digits}{integer specifiying the precision for printing, see
    \code{\link{print.default}}.}
  \item{max.level, digits.d, give.attr, wid, nest.lev, indent.str}{%
    arguments to \code{str}, see \code{\link{str.default}()}.  Note that
    \code{give.attr = FALSE} still shows \code{height} and \code{members}
    attributes for each node.}
  \item{stem}{a string used for \code{str()} specifying the \emph{stem}
    to use for each dendrogram branch.}
}
\details{
  Warning: This documentation is preliminary.

  The dendrogram is directly represented as a nested list where each
  component corresponds to a branch of the tree.  Hence, the first
  branch of tree \code{z} is \code{z[[1]]}, the second branch of the
  corresponding subtree is \code{z[[1]][[2]]} etc.. Each node of the tree
  carries some information needed for efficient plotting or cutting as
  attributes, of which only \code{members}, \code{height} and
  \code{leaf} for leaves are compulsory:
  \describe{
    \item{\code{members}}{total number of leaves in the branch}
    \item{\code{height}}{numeric non-negative height at which the node
      is plotted.}
    \item{\code{midpoint}}{numeric horizontal distance of the node from
      the left border (the leftmost leaf) of the branch (unit 1 between
      all leaves).  This is used for \code{plot(*, center=FALSE)}.}
    \item{\code{label}}{character; the label of the node}
    \item{\code{x.member}}{for \code{cut()$upper},
      the number of \emph{former} members; more generally a substitute
      for the \code{members} component used for \dQuote{horizontal}
      (when \code{horiz = FALSE}, else \dQuote{vertical}) alignment.}
    \item{\code{edgetext}}{character; the label for the edge leading to
      the node}
    \item{\code{nodePar}}{a named list (of length-1 components)
      specifying node-specific attributes for \code{\link{points}}
      plotting, see the \code{nodePar} argument above.}
    \item{\code{edgePar}}{a named list (of length-1 components)
      specifying attributes for \code{\link{segments}} plotting of the
      edge leading to the node, and drawing of the \code{edgetext} if
      available, see the \code{edgePar} argument above.}
    \item{\code{leaf}}{logical, if \code{TRUE}, the node is a leaf of
      the tree.}%  This will often be a \code{\link{character}} which can
    %      be used for plotting instead of the \code{text} attribute.}
  }

  \code{cut.dendrogram()} returns a list with components \code{$upper}
  and \code{$lower}, the first is a truncated version of the original
  tree, also of class \code{dendrogram}, the latter a list with the
  branches obtained from cutting the tree, each a \code{dendrogram}.

  There are \code{\link{[[}}, \code{\link{print}}, and \code{\link{str}}
  methods for \code{"dendrogram"} objects where the first one
  (extraction) ensures that selecting sub-branches keeps the class.

  Objects of class \code{"hclust"} can be converted to class
  \code{"dendrogram"} using method \code{as.dendrogram}.

  \code{rev.dendrogram} simply returns the dendrogram \code{x} with
  reversed nodes, see also \code{\link{reorder.dendrogram}}.

  \code{is.leaf(object)} is logical indicating if \code{object} is a
  leaf (the most simple dendrogram).
  \code{plotNode()} and \code{plotNodeLimit()} are helper functions.
}
\note{
  When using \code{type = "triangle"}, \code{center = TRUE} often looks
  better.
}
\seealso{
  \code{\link{order.dendrogram}} also on the \code{\link{labels}} method
  for dendrograms.
}
\examples{
hc <- hclust(dist(USArrests), "ave")
(dend1 <- as.dendrogram(hc)) # "print()" method
str(dend1)          # "str()" method
str(dend1, max = 2) # only the first two sub-levels

op <- par(mfrow= c(2,2), mar = c(5,2,1,4))
plot(dend1)
## "triangle" type and show inner nodes:
plot(dend1, nodePar=list(pch = c(1,NA), cex=0.8, lab.cex = 0.8),
      type = "t", center=TRUE)
plot(dend1, edgePar=list(col = 1:2, lty = 2:3), dLeaf=1, edge.root = TRUE)
plot(dend1, nodePar=list(pch = 2:1,cex=.4*2:1, col = 2:3), horiz=TRUE)

dend2 <- cut(dend1, h=70)
plot(dend2$upper)
## leafs are wrong horizontally:%% FIXME ?!?
plot(dend2$upper, nodePar=list(pch = c(1,7), col = 2:1))
##  dend2$lower is *NOT* a dendrogram, but a list of .. :
plot(dend2$lower[[3]], nodePar=list(col=4), horiz = TRUE, type = "tr")
## "inner" and "leaf" edges in different type & color :
plot(dend2$lower[[2]], nodePar=list(col=1),# non empty list
     edgePar = list(lty=1:2, col=2:1), edge.root=TRUE)
par(op)
str(d3 <- dend2$lower[[2]][[2]][[1]])

nP <- list(col=3:2, cex=c(2.0, 0.75), pch= 21:22, bg= c("light blue", "pink"),
           lab.cex = 0.75, lab.col = "tomato")
plot(d3, nodePar= nP, edgePar = list(col="gray", lwd=2), horiz = TRUE)
%% now add some "edgetext" :
addE <- function(n) {
      if(!is.leaf(n)) {
        attr(n, "edgePar") <- list(p.col="plum")
        attr(n, "edgetext") <- paste(attr(n,"members"),"members")
      }
      n
}
d3e <- dendrapply(d3, addE)
plot(d3e, nodePar= nP)
plot(d3e, nodePar= nP, leaflab = "textlike")

%% BUG:  edge labeling *and* leaflab="textlike" both fail with  horiz = TRUE:
%% BUG plot(d3e, nodePar= nP, leaflab = "textlike", horiz = TRUE)
}
\keyword{multivariate}
\keyword{tree}% FIXME: want as.dendrogram.tree() etc!
\keyword{hplot}% only for plot.()
