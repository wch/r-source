% File src/library/stats/man/cutree.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{cutree}
\alias{cutree}
\title{Cut a Tree into Groups of Data}
\description{
  Cuts a tree, e.g., as resulting from \code{\link{hclust}}, into several
  groups either by specifying the desired number(s) of groups or the cut
  height(s).
}
\usage{
cutree(tree, k = NULL, h = NULL)
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
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\value{
  \code{cutree} returns a vector with group memberships if \code{k} or
  \code{h} are scalar, otherwise a matrix with group memberships is returned
  where each column corresponds to the elements of \code{k} or \code{h},
  respectively (which are also used as column names).
}
\details{
  Cutting trees at a given height is only possible for ultrametric trees
  (with monotone clustering heights).
}
\seealso{
  \code{\link{hclust}}, \code{\link{dendrogram}} for cutting trees themselves.
}
\examples{
hc <- hclust(dist(USArrests))

cutree(hc, k = 1:5) #k = 1 is trivial
cutree(hc, h = 250)

## Compare the 2 and 4 grouping:
g24 <- cutree(hc, k = c(2,4))
table(grp2 = g24[,"2"], grp4 = g24[,"4"])
}
\keyword{multivariate}
\keyword{cluster}
