\name{heatmap}
\alias{heatmap}
\title{ Draw a Heat Map }
\description{
  A heat map is a false color image with a dendrogram added to the left
  side and to the top.  Typically reordering of the rows and columns
  according to some set of values (row or column means) within the
  restrictions imposed by the dendrogram is carried out.
}
\usage{
heatmap(x, Rowv, Colv, distfun = dist, hclustfun = hclust, add.expr,
        scale=c("row", "column", "none"), na.rm = TRUE, \dots)
}
\arguments{
  \item{x}{numeric matrix of the values to be plotted. }
  \item{Rowv}{either a \code{\link{dendrogram}} or a vector of values
    used to reorder the row dendrogram or by default, missing, see
    \emph{Details} below.}
  \item{Colv}{either a dendrogram or a vector of values used to reorder
    the column dendrogram.}
  \item{distfun}{function used to compute the distance (dissimilarity)
    between both rows and columns.  Defaults to \code{\link{dist}}.}
  \item{hclustfun}{function used to compute the hierarchical clustering
    when \code{Rowv} or \code{Colv} are not dendrograms.  Defaults to
    \code{\link{hclust}}.}
  \item{add.expr}{expression that will be evaluated after the call to
    \code{image}.  Can be used to add components to the plot.}
  \item{scale}{character indicating if the values should be centered and
    scaled in either the row direction or the column direction, or
    none.  The default is \code{"row"}.}
  \item{na.rm}{logical indicating whether \code{NA}'s should be removed.}
  \item{\dots}{additional arguments passed on to \code{\link{image}},
    e.g., \code{col} specifying the colors.} %
}
\details{
  If either \code{Rowv} or \code{Colv} are dendrograms they are honored
  (and not reordered).  Otherwise, dendrograms are computed as
  \code{dd <- as.dendrogram(hclustfun(distfun(X)))} where \code{X} is
  either \code{x} or \code{t(x)}.\cr

  If either is a vector (of ``weights'') then the appropriate dendrogram
  is reordered according to the supplied values subject to the
  constraints imposed by the dendrogram, by \code{\link{reorder}(dd, Rowv)},
  in the row case.  If either is missing, as by default, then the
  ordering of the corresponding dendrogram is by the mean value of the
  rows/columns, i.e., in the case of rows,
  \code{Rowv <- rowMeans(x, na.rm=na.rm)}.

  By default (\code{scale = "row"}) the rows are scaled to have mean
  zero and standard deviation one.  There is some empirical evidence
  from genomic plotting that this is useful.

  The default colors are not pretty.  Consider using RColorBrewer.
}
\value{
  Invisibly, a list with components
  \item{rowInd}{\bold{r}ow index permutation vector as returned by
    \code{\link{order.dendrogram}}.}
  \item{colInd}{\bold{c}olumn index permutation vector.}
}
\author{Andy Liaw, original; R. Gentleman and M. Maechler, revisions.}

\seealso{\code{\link{image}}, \code{\link{hclust}}}

\examples{
  set.seed(132)
  x <- matrix(runif(1000), 50, 20)
  hv <- heatmap(x, col = topo.colors(32), main = "Random U[0,1] heatmap")
  str(hv) # the two re-ordering index vectors

  data(volcano)
  ## Volcano is not recognizable, but at least not random
  heatmap(volcano, col = gray(0:64/64))
}
\keyword{hplot}
