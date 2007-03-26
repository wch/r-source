\name{order.dendrogram}
\alias{order.dendrogram}
\alias{labels.dendrogram}
\title{Ordering or Labels of the Leaves in a Dendrogram}
\description{
  Theses functions return the order (index) or the \code{"label"}
  attribute for the leaves in a
  dendrogram.  These indices can then be used to access the appropriate
  components of any additional data.
}
\usage{
order.dendrogram(x)

\method{labels}{dendrogram}(object, \dots)
}
\arguments{
  \item{x, object}{a dendrogram (see \code{\link{as.dendrogram}}).}
  \item{\dots}{additional arguments}
}
\details{
  The indices or labels for the leaves in left to right order are retrieved.
}
\value{
  A vector with length equal to the number of leaves in the dendrogram
  is returned.  From \code{r <- order.dendrogram()}, each element is the
  index into the original data (from which the dendrogram was computed).
}
\author{R. Gentleman (\code{order.dendrogram} and Martin Maechler
  (\code{labels.dendrogram}).}

\seealso{\code{\link{reorder}}, \code{\link{dendrogram}}.
}
\examples{
set.seed(123)
x <- rnorm(10)
hc <- hclust(dist(x))
hc$order
dd <- as.dendrogram(hc)
order.dendrogram(dd) ## the same :
stopifnot(hc$order == order.dendrogram(dd))

d2 <- as.dendrogram(hclust(dist(USArrests)))
labels(d2) ## in this case the same as
stopifnot(labels(d2) == rownames(USArrests)[order.dendrogram(d2)])
}
\keyword{manip}
