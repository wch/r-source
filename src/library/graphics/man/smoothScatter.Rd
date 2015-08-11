% File src/library/graphics/man/smoothscatter.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2015 R Core Team
% Distributed under GPL 2 or later

\name{smoothScatter}
\alias{smoothScatter}
\title{Scatterplots with Smoothed Densities Color Representation}

\description{\code{smoothScatter} produces a smoothed color density
  representation of a scatterplot, obtained through a (2D) kernel
  density estimate.
}
\usage{
smoothScatter(x, y = NULL, nbin = 128, bandwidth,
              colramp = colorRampPalette(c("white", blues9)),
              nrpoints = 100, ret.selection = FALSE,
              pch = ".", cex = 1, col = "black",
              transformation = function(x) x^.25,
              postPlotHook = box,
              xlab = NULL, ylab = NULL, xlim, ylim,
              xaxs = par("xaxs"), yaxs = par("yaxs"), ...)
}
\arguments{
  \item{x, y}{the \code{x} and \code{y} arguments provide the x and y
    coordinates for the plot.  Any reasonable way of defining the
    coordinates is acceptable.  See the function \code{\link{xy.coords}}
    for details.  If supplied separately, they must be of the same length.}
  \item{nbin}{numeric vector of length one (for both directions) or two
    (for x and y separately) specifying the number of equally spaced
    grid points for the density estimation; directly used as
    \code{gridsize} in \code{\link[KernSmooth]{bkde2D}()}.}
  \item{bandwidth}{numeric vector (length 1 or 2) of smoothing bandwidth(s).
    If missing, a more or less useful default is used. \code{bandwidth}
    is subsequently passed to function
    \code{\link[KernSmooth]{bkde2D}}.}
  \item{colramp}{function accepting an integer \code{n} as an argument and
    returning \code{n} colors.}
  \item{nrpoints}{number of points to be superimposed on the density
    image.  The first \code{nrpoints} points from those areas of lowest
    regional densities will be plotted.  Adding points to the plot
    allows for the identification of outliers.  If all points are to be
    plotted, choose \code{nrpoints = Inf}.}
  \item{ret.selection}{\code{\link{logical}} indicating to return the
    ordered indices of \dQuote{low density} points if \code{nrpoints > 0}.}
  \item{pch, cex, col}{arguments passed to \code{\link{points}},
    when \code{nrpoints > 0}: point symbol, character expansion factor
    and color, see also \code{\link{par}}.}
  \item{transformation}{function mapping the density scale to the color scale.}
  \item{postPlotHook}{either \code{NULL} or a function which will be
    called (with no arguments) after \code{\link{image}}.}
  \item{xlab, ylab}{character strings to be used as axis labels, passed
    to \code{\link{image}}.}
  \item{xlim, ylim}{numeric vectors of length 2 specifying axis limits.}
  \item{xaxs, yaxs, \dots}{further arguments passed to \code{\link{image}},
    e.g., \code{add=TRUE} or \code{useRaster=TRUE}.}
}
\value{
  If \code{ret.selection} is true, a vector of integers of length
  \code{nrpoints} (or smaller, if there are less finite points inside
  \code{xlim} and \code{ylim}) with the indices of the low-density
  points drawn, ordered with lowwest density first.
}
\details{
  \code{smoothScatter} produces a smoothed version of a scatter plot.
  Two dimensional (kernel density) smoothing is performed by
  \code{\link[KernSmooth]{bkde2D}} from package \CRANpkg{KernSmooth}.
  See the examples for how to use this function together with
  \code{\link{pairs}}.
}
\seealso{
  \code{\link[KernSmooth]{bkde2D}} from package \CRANpkg{KernSmooth};
  \code{\link{densCols}} which uses the same smoothing computations and
  \code{\link{blues9}} in package \pkg{grDevices}.

  \code{\link{scatter.smooth}} adds a \code{\link{loess}}
  regression smoother to a scatter plot.
}
\author{Florian Hahne at FHCRC, originally}
\examples{\donttest{
## A largish data set
n <- 10000
x1  <- matrix(rnorm(n), ncol = 2)
x2  <- matrix(rnorm(n, mean = 3, sd = 1.5), ncol = 2)
x   <- rbind(x1, x2)

oldpar <- par(mfrow = c(2, 2), mar=.1+c(3,3,1,1), mgp = c(1.5, 0.5, 0))
smoothScatter(x, nrpoints = 0)
smoothScatter(x)

## a different color scheme:
Lab.palette <- colorRampPalette(c("blue", "orange", "red"), space = "Lab")
i.s <- smoothScatter(x, colramp = Lab.palette,
                     ## pch=NA: do not draw them
                     nrpoints = 250, ret.selection=TRUE)
## label the 20 very lowest-density points,the "outliers" (with obs.number):
i.20 <- i.s[1:20]
text(x[i.20,], labels = i.20, cex= 0.75)

## somewhat similar, using identical smoothing computations,
## but considerably *less* efficient for really large data:
plot(x, col = densCols(x), pch = 20)

## use with pairs:
par(mfrow = c(1, 1))
y <- matrix(rnorm(40000), ncol = 4) + 3*rnorm(10000)
y[, c(2,4)] <-  -y[, c(2,4)]
pairs(y, panel = function(...) smoothScatter(..., nrpoints = 0, add = TRUE),
      gap = 0.2)

par(oldpar)
}}
\keyword{hplot}
