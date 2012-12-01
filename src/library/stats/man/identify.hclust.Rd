% File src/library/stats/man/identify.hclust.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{identify.hclust}
\alias{identify.hclust}
\title{Identify Clusters in a Dendrogram}
\description{
  \code{identify.hclust} reads the position of the graphics pointer when the
  (first) mouse button is pressed.  It then cuts the tree at the
  vertical position of the pointer and highlights the cluster containing
  the horizontal position of the pointer.  Optionally a function is
  applied to the index of data points contained in the cluster.
}
\usage{
\method{identify}{hclust}(x, FUN = NULL, N = 20, MAXCLUSTER = 20, DEV.FUN = NULL,
          \dots)
}
\arguments{
  \item{x}{an object of the type produced by \code{hclust}.}
  \item{FUN}{(optional) function to be applied to the index numbers of
    the data points in a cluster (see \sQuote{Details} below).}
  \item{N}{the maximum number of clusters to be identified.}
  \item{MAXCLUSTER}{the maximum number of clusters that can be produced
    by a cut (limits the effective vertical range of the pointer). }
  \item{DEV.FUN}{(optional) integer scalar. If specified, the
    corresponding graphics device is made active before \code{FUN} is
    applied.}
  \item{\dots}{further arguments to \code{FUN}.}
}
\details{
  By default clusters can be identified using the mouse and an
  \code{\link{invisible}} list of indices of the respective data points
  is returned.

  If \code{FUN} is not \code{NULL}, then the index vector of data points
  is passed to this function as first argument, see the examples
  below.  The active graphics device for \code{FUN} can be specified using
  \code{DEV.FUN}.

  The identification process is terminated by pressing any mouse
  button other than the first, see also \code{\link{identify}}.
}
\value{
  Either a list of data point index vectors or a list of return values
  of \code{FUN}.
}
\seealso{
  \code{\link{hclust}},
  \code{\link{rect.hclust}}
}
\examples{\dontrun{
require(graphics)

hca <- hclust(dist(USArrests))
plot(hca)
(x <- identify(hca)) ##  Terminate with 2nd mouse button !!

hci <- hclust(dist(iris[,1:4]))
plot(hci)
identify(hci, function(k) print(table(iris[k,5])))

# open a new device (one for dendrogram, one for bars):
dev.new() # << make that narrow (& small)
          # and *beside* 1st one
nD <- dev.cur()            # to be for the barplot
dev.set(dev.prev())  # old one for dendrogram
plot(hci)
## select subtrees in dendrogram and "see" the species distribution:
identify(hci, function(k) barplot(table(iris[k,5]), col = 2:4), DEV.FUN = nD)
}}
\keyword{cluster}
\keyword{iplot}
