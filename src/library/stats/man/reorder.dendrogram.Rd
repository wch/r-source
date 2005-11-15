\name{reorder.dendrogram}
\alias{reorder.dendrogram}
\title{Reorder a Dendrogram}
\description{
  A method for the generic function \code{\link{reorder}}.

  There are many different orderings of a dendrogram that are consistent
  with the structure imposed.  This function takes a dendrogram and a
  vector of values and reorders the dendrogram in the order of the
  supplied vector, maintaining the constraints on the dendrogram.
}
\usage{
\method{reorder}{dendrogram}(x, wts, agglo.FUN = sum, \dots)
}
\arguments{
  \item{x}{the (dendrogram) object to be reordered}
  \item{wts}{numeric weights (arbitrary values) for reordering.}
  \item{agglo.FUN}{a function for weights agglomeration, see below.}
  \item{\dots}{additional arguments}
}
\details{
  Using the weights \code{wts}, the leaves of the dendrogram are
  reordered so as to be in an order as consistent as possible with the
  weights.  At each node, the branches are ordered in
  increasing weights where the weight of a branch is defined as
  \eqn{f(w_j)} where \eqn{f} is \code{agglo.FUN} and \eqn{w_j} is the
  weight of the \eqn{j}-th sub branch).
}
\value{
  A dendrogram where each node has a further attribute \code{value} with
  its corresponding weight.
}
\author{R. Gentleman and M. Maechler}
\seealso{
  \code{\link{reorder}}.

  \code{\link{rev.dendrogram}} which simply reverses the nodes'
  order; \code{\link{heatmap}}, \code{\link{cophenetic}}.
}
\examples{
  set.seed(123)
  x <- rnorm(10)
  hc <- hclust(dist(x))
  dd <- as.dendrogram(hc)
  dd.reorder <- reorder(dd, 10:1)
  plot(dd, main = "random dendrogram `dd'")

  op <- par(mfcol = 1:2)
  plot(dd.reorder, main = "reorder(dd, 10:1)")
  plot(reorder(dd,10:1, agglo.FUN= mean),
       main = "reorder(dd, 10:1, mean)")
  par(op)
}
\keyword{manip}
