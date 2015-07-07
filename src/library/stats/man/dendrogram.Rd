% File src/library/stats/man/dendrogram.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2015 R Core Team
% Copyright 2002-2015 The R Foundation
% Distributed under GPL 2 or later

\name{dendrogram}
\title{General Tree Structures}
\alias{dendrogram}% the class
\alias{as.dendrogram}
\alias{as.dendrogram.dendrogram}
\alias{as.dendrogram.hclust}
\alias{as.hclust.dendrogram}
\alias{cut.dendrogram}
\alias{[[.dendrogram}
\alias{merge.dendrogram}
\alias{nobs.dendrogram}
\alias{plot.dendrogram}
\alias{print.dendrogram}
\alias{rev.dendrogram}
\alias{str.dendrogram}
\alias{is.leaf}
\description{
  Class \code{"dendrogram"} provides general functions for handling
  tree-like structures.  It is intended as a replacement for similar
  functions in hierarchical clustering and classification/regression
  trees, such that all of these can use the same engine for plotting or
  cutting trees.
}
\usage{
as.dendrogram(object, \dots)
\method{as.dendrogram}{hclust}(object, hang = -1, check = TRUE, \dots)

\method{as.hclust}{dendrogram}(x, \dots)

\method{plot}{dendrogram}(x, type = c("rectangle", "triangle"),
      center = FALSE,
      edge.root = is.leaf(x) || !is.null(attr(x,"edgetext")),
      nodePar = NULL, edgePar = list(),
      leaflab = c("perpendicular", "textlike", "none"),
      dLeaf = NULL, xlab = "", ylab = "", xaxt = "n", yaxt = "s",
      horiz = FALSE, frame.plot = FALSE, xlim, ylim, \dots)

\method{cut}{dendrogram}(x, h, \dots)

\method{merge}{dendrogram}(x, y, \dots, height,
      adjust = c("auto", "add.max", "none"))

\method{nobs}{dendrogram}(object, \dots)

\method{print}{dendrogram}(x, digits, \dots)

\method{rev}{dendrogram}(x)

\method{str}{dendrogram}(object, max.level = NA, digits.d = 3,
    give.attr = FALSE, wid = getOption("width"),
    nest.lev = 0, indent.str = "",
    last.str = getOption("str.dendrogram.last"), stem = "--",
    \dots)

is.leaf(object)
}
\arguments{
  \item{object}{any \R object that can be made into one of class
    \code{"dendrogram"}.}
  \item{x, y}{object(s) of class \code{"dendrogram"}.}
  \item{hang}{numeric scalar indicating how the \emph{height} of leaves
    should be computed from the heights of their parents; see
    \code{\link{plot.hclust}}.}
  \item{check}{logical indicating if \code{object} should be checked for
    validity.  This check is not necessary when \code{x} is known to be
    valid such as when it is the direct result of \code{hclust()}.  The
    default is \code{check=TRUE}, e.g. for protecting against memory
    explosion with invalid inputs.}
  \item{type}{type of plot.}
  \item{center}{logical; if \code{TRUE}, nodes are plotted centered with
    respect to the leaves in the branch.  Otherwise (default), plot them
    in the middle of all direct child nodes.}
  \item{edge.root}{logical; if true, draw an edge to the root node.}
  \item{nodePar}{a \code{list} of plotting parameters to use for the
    nodes (see \code{\link{points}}) or \code{NULL} by default which
    does not draw symbols at the nodes.  The list may contain components
    named \code{pch}, \code{cex}, \code{col}, \code{xpd},
    and/or \code{bg} each of
    which can have length two for specifying separate attributes for
    \emph{inner} nodes and \emph{leaves}.  Note that the default of
    \code{pch} is \code{1:2}, so you may want to use \code{pch = NA} if
    you specify \code{nodePar}.}
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
  \item{height}{height at which the two dendrograms should be merged.  If not
    specified (or \code{NULL}), the default is ten percent larger than
    the (larger of the) two component heights.}
  \item{adjust}{a string determining if the leaf values should be
    adjusted.  The default, \code{"auto"}, checks if the (first) two
    dendrograms both start at \code{1}; if they do, code{"add.max"} is
    chosen, which adds the maximum of the previous dendrogram leaf
    values to each leaf of the \dQuote{next} dendrogram.  Specifying
    \code{adjust} to another value skips the check and hence is a tad
    more efficient.}
  \item{xlim, ylim}{optional x- and y-limits of the plot, passed to
    \code{\link{plot.default}}.  The defaults for these show the full
    dendrogram.}
  \item{\dots, xlab, ylab, xaxt, yaxt}{graphical parameters, or arguments for
    other methods.}
  \item{digits}{integer specifying the precision for printing, see
    \code{\link{print.default}}.}
  \item{max.level, digits.d, give.attr, wid, nest.lev, indent.str}{arguments
    to \code{str}, see \code{\link{str.default}()}.  Note that
    \code{give.attr = FALSE} still shows \code{height} and \code{members}
    attributes for each node.}
  \item{last.str, stem}{strings used for \code{str()} specifying how the
    last branch (at each level) should start and the \emph{stem}
    to use for each dendrogram branch.  In some environments, using
    \code{last.str = "'"} will provide much nicer looking output, than
    the historical default \code{last.str = "`"}.}
}
\details{
  The dendrogram is directly represented as a nested list where each
  component corresponds to a branch of the tree.  Hence, the first
  branch of tree \code{z} is \code{z[[1]]}, the second branch of the
  corresponding subtree is \code{z[[1]][[2]]}, or shorter
  \code{z[[c(1,2)]]}, etc..  Each node of the tree
  carries some information needed for efficient plotting or cutting as
  attributes, of which only \code{members}, \code{height} and
  \code{leaf} for leaves are compulsory:
  \describe{
    \item{\code{members}}{total number of leaves in the branch}
    \item{\code{height}}{numeric non-negative height at which the node
      is plotted.}
    \item{\code{midpoint}}{numeric horizontal distance of the node from
      the left border (the leftmost leaf) of the branch (unit 1 between
      all leaves).  This is used for \code{plot(*, center = FALSE)}.}
    \item{\code{label}}{character; the label of the node}
    \item{\code{x.member}}{for \code{cut()$upper},
      the number of \emph{former} members; more generally a substitute
      for the \code{members} component used for \sQuote{horizontal}
      (when \code{horiz = FALSE}, else \sQuote{vertical}) alignment.}
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
  (extraction) ensures that selecting sub-branches keeps the class,
  i.e., returns a dendrogram even if only a leaf.
  On the other hand, \code{\link{[}} (\emph{single} bracket) extraction
  returns the underlying list structure.%, useful, e.g., for inspection.

  Objects of class \code{"hclust"} can be converted to class
  \code{"dendrogram"} using method \code{as.dendrogram()}, and since R
  2.13.0, there is also a \code{\link{as.hclust}()} method as an inverse.

  \code{rev.dendrogram} simply returns the dendrogram \code{x} with
  reversed nodes, see also \code{\link{reorder.dendrogram}}.

  The \code{\link{merge}(x, y, ...)} method merges two or more
  dendrograms into a new one which has \code{x} and \code{y} (and
  optional further arguments) as branches.  Note that before \R 3.1.2,
  \code{adjust = "none"} was used implicitly, which is invalid when,
  e.g., the dendrograms are from \code{\link{as.dendrogram}(hclust(..))}.

  \code{\link{nobs}(object)} returns the total number of leaves (the
  \code{members} attribute, see above).

  \code{is.leaf(object)} returns logical indicating if \code{object} is a
  leaf (the most simple dendrogram).

  \code{plotNode()} and \code{plotNodeLimit()} are helper functions.
}
\note{
  \describe{
    \item{\code{plot()}:}{When using \code{type = "triangle"},
      \code{center = TRUE} often looks better.}
    \item{\code{str(d)}:}{If you really want to see the \emph{internal}
      structure, use \code{str(unclass(d))} instead.}
  }
}
\section{Warning}{
  Some operations on dendrograms such as \code{merge()} make use of
  recursion.  For very deep trees it may be necessary to increase
  \code{\link{options}("expressions")}: if you do you are likely to need
  to set the C stack size larger than the default where possible.
}
\seealso{
  \code{\link{dendrapply}} for applying a function to \emph{each} node.
  \code{\link{order.dendrogram}} and \code{\link{reorder.dendrogram}};
  further, the \code{\link{labels}} method.
}
\examples{
require(graphics); require(utils)

hc <- hclust(dist(USArrests), "ave")
(dend1 <- as.dendrogram(hc)) # "print()" method
str(dend1)          # "str()" method
str(dend1, max = 2, last.str =  "'") # only the first two sub-levels
oo <- options(str.dendrogram.last = "\\\\") # yet another possibility
str(dend1, max = 2) # only the first two sub-levels
options(oo)  # .. resetting them

op <- par(mfrow =  c(2,2), mar = c(5,2,1,4))
plot(dend1)
## "triangle" type and show inner nodes:
plot(dend1, nodePar = list(pch = c(1,NA), cex = 0.8, lab.cex = 0.8),
      type = "t", center = TRUE)
plot(dend1, edgePar = list(col = 1:2, lty = 2:3),
     dLeaf = 1, edge.root = TRUE)
plot(dend1, nodePar = list(pch = 2:1, cex = .4*2:1, col = 2:3),
     horiz = TRUE)

## simple test for as.hclust() as the inverse of as.dendrogram():
stopifnot(identical(as.hclust(dend1)[1:4], hc[1:4]))

dend2 <- cut(dend1, h = 70)
plot(dend2$upper)
## leaves are wrong horizontally:
plot(dend2$upper, nodePar = list(pch = c(1,7), col = 2:1))
##  dend2$lower is *NOT* a dendrogram, but a list of .. :
plot(dend2$lower[[3]], nodePar = list(col = 4), horiz = TRUE, type = "tr")
## "inner" and "leaf" edges in different type & color :
plot(dend2$lower[[2]], nodePar = list(col = 1),   # non empty list
     edgePar = list(lty = 1:2, col = 2:1), edge.root = TRUE)
par(op)
d3 <- dend2$lower[[2]][[2]][[1]]
stopifnot(identical(d3, dend2$lower[[2]][[c(2,1)]]))
str(d3, last.str = "'")

## to peek at the inner structure "if you must", use '[..]' indexing :
str(d3[2][[1]]) ## or the full
str(d3[])

## merge() to join dendrograms:
(d13 <- merge(dend2$lower[[1]], dend2$lower[[3]]))
## merge() all parts back (using default 'height' instead of original one):
den.1 <- Reduce(merge, dend2$lower)
## or merge() all four parts at same height --> 4 branches (!)
d. <- merge(dend2$lower[[1]], dend2$lower[[2]], dend2$lower[[3]],
            dend2$lower[[4]])
## (with a warning) or the same using  do.call :
stopifnot(identical(d., do.call(merge, dend2$lower)))
plot(d., main = "merge(d1, d2, d3, d4)  |->  dendrogram with a 4-split")

## "Zoom" in to the first dendrogram :
plot(dend1, xlim = c(1,20), ylim = c(1,50))

nP <- list(col = 3:2, cex = c(2.0, 0.75), pch =  21:22,
           bg =  c("light blue", "pink"),
           lab.cex = 0.75, lab.col = "tomato")
plot(d3, nodePar= nP, edgePar = list(col = "gray", lwd = 2), horiz = TRUE)
%% now add some "edgetext" :
addE <- function(n) {
      if(!is.leaf(n)) {
        attr(n, "edgePar") <- list(p.col = "plum")
        attr(n, "edgetext") <- paste(attr(n,"members"),"members")
      }
      n
}
d3e <- dendrapply(d3, addE)
plot(d3e, nodePar =  nP)
plot(d3e, nodePar =  nP, leaflab = "textlike")

%% BUG:  edge labeling *and* leaflab = "textlike" both fail with  horiz = TRUE:
%% BUG plot(d3e, nodePar = nP, leaflab = "textlike", horiz = TRUE)
}
\keyword{multivariate}
\keyword{tree}% FIXME: want as.dendrogram.tree() etc!
\keyword{hplot}% only for plot.()
