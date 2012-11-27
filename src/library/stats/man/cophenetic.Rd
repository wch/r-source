% File src/library/stats/man/cophenetic.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{cophenetic}
\alias{cophenetic}
\alias{cophenetic.default}
\alias{cophenetic.dendrogram}
\title{Cophenetic Distances for a Hierarchical Clustering}
\description{
  Computes the cophenetic distances for a hierarchical clustering.
}
\usage{
cophenetic(x)
\method{cophenetic}{default}(x)
\method{cophenetic}{dendrogram}(x)
}
\arguments{
  \item{x}{an R object representing a hierarchical clustering.
    For the default method, an object of class \code{"\link{hclust}"} or
    with a method for \code{\link{as.hclust}()} such as
    \code{"\link[cluster]{agnes}"} in package \CRANpkg{cluster}.}
}
\details{
  The cophenetic distance between two observations that have been
  clustered is defined to be the intergroup dissimilarity at which the
  two observations are first combined into a single cluster.
  Note that this distance has many ties and restrictions.

  It can be argued that a dendrogram is an appropriate summary of some
  data if the correlation between the original distances and the
  cophenetic distances is high.  Otherwise, it should simply be viewed as
  the description of the output of the clustering algorithm.

  \code{cophenetic} is a generic function.  Support for classes which
  represent hierarchical clusterings (total indexed hierarchies) can be
  added by providing an \code{\link{as.hclust}()} or, more directly, a
  \code{cophenetic()} method for such a class.

  The method for objects of class \code{"\link{dendrogram}"} requires
  that all leaves of the dendrogram object have non-null labels.
}
\value{
  An object of class \code{"dist"}.
}
\references{
  Sneath, P.H.A. and Sokal, R.R. (1973)
  \emph{Numerical Taxonomy: The Principles and Practice of Numerical
    Classification}, p. 278 ff;
  Freeman, San Francisco.
}
\author{Robert Gentleman}
\seealso{
  \code{\link{dist}},
  \code{\link{hclust}}
}
\examples{
require(graphics)

d1 <- dist(USArrests)
hc <- hclust(d1, "ave")
d2 <- cophenetic(hc)
cor(d1, d2) # 0.7659

## Example from Sneath & Sokal, Fig. 5-29, p.279
d0 <- c(1,3.8,4.4,5.1, 4,4.2,5, 2.6,5.3, 5.4)
attributes(d0) <- list(Size = 5, diag = TRUE)
class(d0) <- "dist"
names(d0) <- letters[1:5]
d0
utils::str(upgma <- hclust(d0, method = "average"))
plot(upgma, hang = -1)
#
(d.coph <- cophenetic(upgma))
cor(d0, d.coph) # 0.9911
}
\keyword{cluster}
\keyword{multivariate}
