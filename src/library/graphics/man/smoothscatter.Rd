\name{smoothScatter}
\alias{smoothScatter}
\alias{densCols}
\alias{blues}
\title{Scatterplots with smoothed densities color representation}

\description{\code{smoothScatter} produces a smoothed color density
  representation of the scatterplot, obtained through a kernel
  density estimate. \code{densCols} produces a vector containing
  colors which encode the local densities at each point in a
  scatterplot.}

\usage{
smoothScatter(x, y=NULL,
              nbin=128,
              bandwidth,
              colramp=colorRampPalette(c("white", brewer.pal(9, "Blues"))),
              nrpoints=100,
              transformation=function(x) x^.25,
              xlab=NULL, ylab=NULL, postPlotHook=box,
              pch=".", cex=1,
              xlim, ylim, col="black",
              xaxs=par("xaxs"), yaxs=par("yaxs"), ...) 

     densCols(x, y=NULL,
              nbin=128,
              bandwidth,
              colramp=colorRampPalette(brewer.pal(9, "Blues")[-(1:3)]))
     blues
}
\arguments{
\item{x}{Numeric vector containing x-values or n by 2 matrix containing x
  and y values.}
\item{y}{Numeric vector containing y-values (optional). The length of \code{x}
  must be the same as that of \code{y}.}
\item{nbin}{Numeric vector of length 1 (for both directions) or
  2 (for x and y separately) containing the number of equally spaced
  grid points for the density estimation.}
\item{bandwidth}{Numeric vector: the smoothing bandwidth. If missing,
  these functions come up with a more or less useful guess. This
  parameter then gets passed on to the function
  \code{\link[KernSmooth::bkde2D]{bkde2D}}.}
\item{colramp}{Function accepting an integer \code{n} as an argument and
  returning \code{n} colors.}
\item{nrpoints}{Numeric vector of length 1 giving number of points to
  be superimposed on the density image. The first \code{nrpoints} points
  from those areas of lowest regional densities will be plotted. Adding
  points to the plot allows for the identification of outliers.
  If all points are to be plotted, choose \code{nrpoints = Inf}.}
\item{transformation}{Function that maps the density scale to the color scale.}
\item{xlab, ylab}{Character. Gets passed on to \code{\link{image}}}
\item{postPlotHook}{Either \code{NULL} or a function with no
  arguments that will be called after \code{\link{image}}.}
\item{pch}{\code{\link{points}} parameter setting}
\item{cex}{character expansion parameter setting
   (see \code{\link{par}}) passed to the \code{\link{points}} phase
   of the function}
 \item{xlim, ylim}{Numeric vectors of length 2, axis limits.}
 \item{col}{\code{\link{points}} color parameter}
\item{xaxs, yaxs, \dots}{Further arguments that are passed on to \code{\link{image}}.}
}

\details{
\code{smoothScatter} produces a smoothed version of a scatter plot.
Two dimensional smoothing is performed using
\code{\link[KernSmooth::bkde2D]{bkde2D}}.
The treatment of the \code{x} and \code{y} arguments attempts to be
consistent with that e.g. of \code{\link{plot.default}}.
See examples for how to use this function together withn \code{\link{pairs}}.

\code{blues} is a set of values of blue used as the default in plotting.

\code{densCols} computes and returns the set of colors that will be used
in plotting.
}

\value{\code{smoothScatter} is called for its side-effect, producing a
  plot on the current graphics device. \code{densCols} returns
  a vector of length \code{nrow(x)} that contains colors to be used
  in a subsequent scatterplot. Each color represents the local density
  around the corresponding point.  
}

\seealso{\code{\link[KernSmooth::bkde2D]{bkde2D}}}

\author{Florian Hahne <fhahne@fhcrc.org>}

\keyword{hplot}

\examples{
  x1  <- matrix(rnorm(1e4), ncol=2)
  x2  <- matrix(rnorm(1e4, mean=3, sd=1.5), ncol=2)
  x   <- rbind(x1,x2)

  oldpar <- par(mfrow=c(2,2))
  smoothScatter(x, nrpoints=0)
  smoothScatter(x)

  colors  <- densCols(x)
  plot(x, col=colors, pch=20)

  ## use with pairs:
  par(mfrow=c(1,1))
  y <- matrix(rnorm(40000), ncol=4) + 3*rnorm(10000)
  y[, c(2,4)] <- (-y[, c(2,4)])
  pairs(y, panel=function(...) {par(new=TRUE);smoothScatter(..., nrpoints=0)})

  par(oldpar)
}



