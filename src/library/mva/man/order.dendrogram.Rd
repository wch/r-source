\name{order.dendrogram}
\alias{order.dendrogram}
\title{ Get the ordering of the leaves in a dendrogram }
\description{
  This function returns the order (index) for the leaves in a
  dendrogram. These indices can then be used to access the appropriate
  components of any additional data.
}
\usage{
order.dendrogram(x)
}
\arguments{
  \item{x}{A dendrogram. }
}
\details{
  The indices for the leaves in left to right order are retrieved.
}
\value{
  A vector with length equal to the number of leaves in the dendrogram
  is returned. Each element is the index into the original data (from
  which the dendrogram was computed)
}
\author{R. Gentleman}

\seealso{\code{\link{reorder}} }

\examples{
  set.seed(123)
  x<-rnorm(10)
  hc <- hclust(dist(x))
  hc$order
  dd <- as.dendrogram(hc)
  order.dendrogram(dd)
}
\keyword{manip }
