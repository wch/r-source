\name{cutree}
\alias{cutree}
\title{Cut a tree into groups of data}
\description{
  Cuts a tree, e.g., as resulting from \code{\link{hclust}}, into several
  groups either by specifying the desired number(s) of groups or the cut
  height(s).
}
\usage{
cutree(tree, k=NULL, h=NULL)
}
\arguments{
 \item{tree}{a tree as produced by \code{\link{hclust}}. \code{cutree()}
   only expects a list with components \code{merge}, \code{height}, and
   \code{labels}, of appropriate content each.}
 \item{k}{an integer scalar or vector with the desired number of groups}
 \item{h}{numeric scalar or vector with heights where the tree should
   be cut.}
 At least one of \code{k} or \code{h} must be specified, \code{k}
 overrides \code{h} if both are given.
}
\value{
  \code{cutree} returns a vector with group memberships if \code{k} or
  \code{h} are scalar, otherwise a matrix with group meberships is returned
  where each column corresponds to the elements of \code{k} or \code{h},
  respectively (which are also used as column names).
}
\seealso{\code{\link{hclust}}}
\examples{
require(mva)
data(USArrests)

hc <- hclust(dist(USArrests))

cutree(hc, k=1:5)#k = 1 is trivial
cutree(hc, h=250)

## Compare the 2 and 3 grouping:
g24 <- cutree(hc, k = c(2,4))
table(g24[,"2"], g24[,"4"])
\testonly{
  ct <- cutree(hc, h = c(0, hc$height[c(1,49)], 1000))
  stopifnot(ct[,"0"]== 1:50,
     unique(ct[,2]) == 1:49,
            ct[,3]  == ct[,4],
            ct[,4]  == 1)
}
}
\keyword{multivariate}
\keyword{cluster}

