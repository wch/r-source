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
heatmap(x, Rowv=NULL, Colv=NULL, distfun = dist, hclustfun = hclust, add.expr,
        scale=c("row", "column", "none"), na.rm = TRUE,
        margins = c(5, 5), ColSideColors, RowSideColors,
        cexRow = 0.2 + 1/log10(nr), cexCol = 0.2 + 1/log10(nc),
        main = NULL, xlab = NULL, ylab = NULL, \dots)
}
\arguments{
  \item{x}{numeric matrix of the values to be plotted. }
  \item{Rowv}{determines if and how the \emph{row} dendrogram should be
    reordered.  Either a \code{\link{dendrogram}} or a vector of values
    used to reorder the row dendrogram or \code{\link{FALSE}} to
    suppress reordering or by default, \code{\link{NULL}}, see
    \emph{Details} below.}
  \item{Colv}{determines if and how the \emph{column} dendrogram should be
    reordered.  For the possibilities, see the \code{Rowv} argument above.}
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
  \item{margins}{numeric vector of length 2 containing the margins
    (see \code{\link{par}(mar= *)}) for column and row names, respectively.}
  \item{ColSideColors}{(optional) character vector of length \code{ncol(x)}
    containing the color names for a horizontal side bar that may be used to
    annotate the columns of \code{x}.}
  \item{RowSideColors}{(optional) character vector of length \code{nrow(x)}
    containing the color names for a vertical side bar that may be used to
    annotate the rows of \code{x}.}
  \item{cexRow, cexCol}{positive numbers, used as \code{cex.axis} in
    for the row or column axis labeling.  The defaults currently only
    use number of rows or columns, respectively.}
  \item{main, xlab, ylab}{main, x- and y-axis titles; defaults to none.}
  \item{\dots}{additional arguments passed on to \code{\link{image}},
    e.g., \code{col} specifying the colors.} %
}
\details{
  If either \code{Rowv} or \code{Colv} are dendrograms they are honored
  (and not reordered).  Otherwise, dendrograms are computed as
  \code{dd <- as.dendrogram(hclustfun(distfun(X)))} where \code{X} is
  either \code{x} or \code{t(x)}.\cr

  If either is a vector (of \dQuote{weights}) then the appropriate
  dendrogram is reordered according to the supplied values subject to
  the constraints imposed by the dendrogram, by \code{\link{reorder}(dd,
    Rowv)}, in the row case.
  %%
  If either is missing, as by default, then the ordering of the
  corresponding dendrogram is by the mean value of the rows/columns,
  i.e., in the case of rows, \code{Rowv <- rowMeans(x, na.rm=na.rm)}.
  %%
  If either is \code{\link{NULL}}, \emph{no reordering} will be done for
  the corresponding side.

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
\author{Andy Liaw, original; R. Gentleman, M. Maechler, W. Huber, revisions.}

\seealso{\code{\link{image}}, \code{\link{hclust}}}

\examples{
 data(mtcars)
 x  <- as.matrix(mtcars)
 rc <- rainbow(nrow(x), start=0, end=.3)
 cc <- rainbow(ncol(x), start=0, end=.3)
 hv <- heatmap(x, col = cm.colors(256), scale="column",
               RowSideColors = rc, ColSideColors = cc, margin=c(5,10),
               xlab = "specification variables", ylab= "Car Models",
               main = "heatmap(<Mtcars data>, ..., scale = \"column\")")
 str(hv) # the two re-ordering index vectors

%% want example using the `add.exp' argument!

%% other example with no ordering !!

 data(volcano)
 ## Volcano is not recognizable, but at least not random
 heatmap(volcano, col = gray(0:64/64))
}
\keyword{hplot}
