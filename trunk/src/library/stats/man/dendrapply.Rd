\name{dendrapply}
\alias{dendrapply}
\title{Apply a Function to All Nodes of a Dendrogram}
\description{
  Apply function \code{FUN} to each node of a \code{\link{dendrogram}}
  recursively.  When  \code{y <- dendrapply(x, fn)}, then \code{y} is a
  dendrogram of the same graph structure as \code{x} and each for each node,
  \code{y.node[j] <- FUN( x.node[j], ...)} (where \code{y.node[j]} is an
  (invalid!) notation for the j-th node of y.
}
\usage{
dendrapply(X, FUN, ...)
}
\arguments{
  \item{X}{an object of class \code{"\link{dendrogram}"}.}
  \item{FUN}{an \R function to be applied to each dendrogram node,
    typically working on its \code{\link{attributes}} alone, returning an
    altered version of the same node.}
  \item{\dots}{potential further arguments passed to \code{FUN}.}
}
\value{
  Usually a dendrogram of the same (graph) structure as \code{X}.
  For that, the function must be conceptually of the form
  \code{FUN <- function(X) { attributes(X) <- .....;  X }},
  i.e. returning the node with some attributes added or changed.
}
\author{Martin Maechler}
\note{this is still somewhat experimental, and suggestions for
  enhancements (or nice examples of usage) are very welcome.}
\seealso{\code{\link{as.dendrogram}}, \code{\link{lapply}} for applying
  a function to each component of a  \code{list}.
}
\examples{
## a smallish simple dendrogram
dhc <- as.dendrogram(hc <- hclust(dist(USArrests), "ave"))
(dhc21 <- dhc[[2]][[1]])

## too simple:
dendrapply(dhc21, function(n) str(attributes(n)))

## toy example to set colored leaf labels :
local({
  colLab <<- function(n) {
      if(is.leaf(n)) {
        a <- attributes(n)
        i <<- i+1
        attr(n, "nodePar") <-
            c(a$nodePar, list(lab.col = mycols[i], lab.font= i\%\%3))
      }
      n
  }
  mycols <- grDevices::rainbow(attr(dhc21,"members"))
  i <- 0
 })
dL <- dendrapply(dhc21, colLab)
op <- par(mfrow=2:1)
 plot(dhc21)
 plot(dL) ## --> colored labels!
par(op)
}
\keyword{iteration}
