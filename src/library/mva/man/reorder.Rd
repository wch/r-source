\name{reorder}
\alias{reorder}
\alias{reorder.dendrogram}
\title{ Reorder a dendrogram }
\description{
  There are many different orderings of a dendrogram that are consistent
  with the structure imposed. This function takes a dendrogram and a
  vector of values and reorders the dendrogram in the order of the
  supplied vector, maintaining the constraints on the dendrogram.
}
\usage{
reorder(x, \dots)
reorder.dendrogram(x, wts, \dots)
}
\arguments{
  \item{x}{The object to be reordered}
  \item{wts}{The weights for reordering.}
  \item{\dots}{additional arguments }
}
\details{
  Using the weights the leaves of the dendrogram are reordered so as to
  be in an order as consistent as possible with the weights.
}
\value{
  From \code{reorder.dendrogram} a dendrogram.
}
\author{R. Gentleman}
\seealso{\code{\link{heatmap}}, \code{\link{cophenetic}} }

\examples{
  set.seed(123)
  x <- rnorm(10)
  hc <- hclust(dist(x))
  dd <- as.dendrogram(hc)
  dd.reorder <- reorder(dd, 10:1)
}
\keyword{manip }
