\name{cutree}
\alias{cutree}
\title{Cut a tree into groups of data}
\description{
  Cuts a tree, e.g., as resulting from \code{\link{hclust}}, into several
  groups either by specifying the desired number of groups or the cut
  height.
}
\usage{
cutree(tree, k=NULL, h=NULL)
}
\arguments{
 \item{tree}{a tree as produced by \code{\link{hclust}}} 
 \item{k}{An integer scalar or vector with the desired number of groups}
 \item{h}{A numeric scalar or vector with heights where the tree should
   be cut.}
 At least one of \code{k} or \code{h} must be specified, \code{k}
 overrides \code{h} if both are given.
}
\value{
  \code{cutree} returns an vector with group meberships if \code{k} or
  \code{h} are scalar, otherwise a matrix with group meberships is returned
  where each column corresponds to the elements of \code{k} or \code{h},
  respectively (which are also used as column names).
}
\seealso{\code{\link{hclust}}}
\examples{
require(mva)
data(USArrests)

hc <- hclust(dist(USArrests))

cutree(hc, k=2:5)
cutree(hc, h=250)
}
\keyword{multivariate}
\keyword{cluster}

