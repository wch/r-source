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
heatmap(x, Rowv, Colv, distfun = dist, add.expr,
        scale=c("row", "column", "none"), \dots)
}
\arguments{
  \item{x}{numeric matrix of the values to be plotted. }
  \item{Rowv}{Either a \code{\link{dendrogram}} or a vector of values
    used to reorder the row dendrogram. }
  \item{Colv}{Either a dendrogram or a vector of values used to reorder
    the column dendrogram. }
  \item{distfun}{function used to compute the distance (dissimilarity)
    between both rows and columns.  Defaults to \code{\link{dist}}.}
  \item{add.expr}{expression that will be evaluated after the call to
    image.  Can be used to add components to the plot.}
  \item{scale}{character indicating if the values should be centered and
    scaled in either the row direction or the column direction, or
    none.  The default is \code{"row"}.}
  \item{\dots}{additional arguments passed on to \code{\link{image}},
    e.g. \code{col} specifying the colors.} %
}
\details{
  If either \code{Rowv} or \code{Colv} are dendrograms they are honored
  (and not reordered).  If either is a vector then the apropriate
  dendrogram is reordered according to the supplied values, subject to
  the constraints imposed by the dendrogram.  If either is missing, as
  by default, then the ordering of the corresponding dendrogram is by
  the mean value of the rows/columns.

  By default the rows are scaled to have mean zero and standard
  deviation one.  There is some empirical evidence from genomic plotting
  that this is useful.

  The default colors are not pretty. Consider using RColorBrewer.
}
\value{
  No value is returned. The function is called for the side effect of
  producing a picture.
}

\author{Andy Liaw, original, R. Gentleman, revisions}

\seealso{\code{\link{image}}}

\examples{
  set.seed(132)
  x <- matrix(runif(1000), 50, 20)
  heatmap(x, col = topo.colors(32), main = "Random U[0,1] heatmap")

  data(volcano)
  ## Volcano is not recognizable, but at least not random
  heatmap(volcano, col = gray(0:64/64))
}
\keyword{hplot }
