\name{cophenetic}
\alias{cophenetic}
\title{Cophenetic Distances for a Hierarchical Clustering}
\description{
  Computes the cophenetic distances for a hierarchical clustering.
}
\usage{
cophenetic(x)
}
\arguments{
  \item{x}{an object of class \code{\link[mva]{hclust}} or with a method
    for \code{\link[mva]{as.hclust}()} such as \code{\link[cluster]{agnes}}.}
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
}
\value{
  An object of class \code{dist}.
}
\references{
  Sneath, P.H.A. and Sokal, R.R (1973)
  \emph{Numerical Taxonomy: The Principles and Practice of Numerical
    Classification}, p. 278 ff;
  Freeman, San Francisco.
}
\author{Robert Gentleman}

\seealso{\code{\link{dist}}, \code{\link{hclust}}}

\examples{
 data(USArrests)
 library(mva)
 d1 <- dist(USArrests)
 hc <- hclust(d1, "ave")
 d2 <- cophenetic(hc)
 cor(d1,d2)
}
\keyword{cluster}
\keyword{multivariate}

