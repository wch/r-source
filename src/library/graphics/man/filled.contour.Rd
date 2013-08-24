% File src/library/graphics/man/filled.contour.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\newcommand{\CRANpkg}{\href{http://CRAN.R-project.org/package=#1}{\pkg{#1}}}

\name{filled.contour}
\alias{filled.contour}
\alias{.filled.contour}

\title{Level (Contour) Plots}
\usage{
filled.contour(x = seq(0, 1, length.out = nrow(z)),
               y = seq(0, 1, length.out = ncol(z)),
               z,
               xlim = range(x, finite = TRUE),
               ylim = range(y, finite = TRUE),
               zlim = range(z, finite = TRUE),
               levels = pretty(zlim, nlevels), nlevels = 20,
               color.palette = cm.colors,
               col = color.palette(length(levels) - 1),
               plot.title, plot.axes, key.title, key.axes,
               asp = NA, xaxs = "i", yaxs = "i", las = 1,
               axes = TRUE, frame.plot = axes, \dots)

.filled.contour(x, y, z, levels, col)
}
\arguments{
 \item{x, y}{locations of grid lines at which the values in \code{z} are
   measured.  These must be in ascending order.  (The rest of this
   description does not apply to \code{.filled.contour}.)
   By default, equally spaced values from 0 to 1 are used.  If \code{x}
   is a \code{list}, its components \code{x$x} and \code{x$y} are used
   for \code{x} and \code{y}, respectively.  If the list has component
   \code{z} this is used for \code{z}.}
 \item{z}{a numeric matrix containing the values to be plotted..  Note that
   \code{x} can be used instead of \code{z} for convenience.}
 \item{xlim}{x limits for the plot.}
 \item{ylim}{y limits for the plot.}
 \item{zlim}{z limits for the plot.}
 \item{levels}{a set of levels which are used to partition the range
    of \code{z}.  Must be \bold{strictly} increasing (and finite).  Areas
    with \code{z} values between consecutive levels are painted with the
    same color.}
 \item{nlevels}{if \code{levels} is not specified, the range of \code{z},
    values is divided into approximately this many levels.}
 \item{color.palette}{a color palette function to be used to assign
    colors in the plot.}
 \item{col}{an explicit set of colors to be used in the plot.
    This argument overrides any palette function specification.  There
    should be one less color than levels}
 \item{plot.title}{statements which add titles to the main plot.}
 \item{plot.axes}{statements which draw axes (and a \code{\link{box}})
   on the main plot.  This overrides the default axes.}
 \item{key.title}{statements which add titles for the plot key.}
 \item{key.axes}{statements which draw axes on the plot key.
    This overrides the default axis.}
 \item{asp}{the \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.}
 \item{xaxs}{the x axis style.  The default is to use internal
    labeling.}
 \item{yaxs}{the y axis style.  The default is to use internal
    labeling.}
 \item{las}{the style of labeling to be used.  The default is to
    use horizontal labeling.}
 \item{axes, frame.plot}{logicals indicating if axes and a box should be
   drawn, as in \code{\link{plot.default}}.}
 \item{\dots}{additional \link{graphical parameters}, currently only passed to
   \code{\link{title}()}.}
}
\description{
  This function produces a contour plot with the areas between the
  contours filled in solid color (Cleveland calls this a level plot).  A
  key showing how the colors map to z values is shown to the right of
  the plot.
}
\details{
  The values to be plotted can contain \code{NA}s.  Rectangles with two
  or more corner values are \code{NA} are omitted entirely: where there
  is a single \code{NA} value the triangle opposite the \code{NA} is
  omitted.

  Values to be plotted can be infinite: the effect is similar to that
  described for \code{NA} values.

  \code{.filled.contour} is a \sQuote{bare bones} interface to add
  just the contour plot to an already-set-up plot region.  It is is
  intended for programmatic use, and the programmer is
  responsible for checking the conditions on the arguments.
}
\references{
  Cleveland, W. S. (1993)
  \emph{Visualizing Data}.
  Summit, New Jersey: Hobart.
}
\author{Ross Ihaka and R-core.}
\note{
  \code{filled.contour} uses the \code{\link{layout}} function and so is
  restricted to a full page display.

  The output produced by \code{filled.contour} is actually a combination
  of two plots; one is the filled contour and one is the legend.  Two
  separate coordinate systems are set up for these two plots, but they
  are only used internally -- once the function has returned these
  coordinate systems are lost.  If you want to annotate the main contour
  plot, for example to add points, you can specify graphics commands in
  the \code{plot.axes} argument.  See the examples.
}
\seealso{
  \code{\link{contour}}, \code{\link{image}},
  \code{\link{palette}}; \code{\link[lattice:levelplot]{contourplot}}
  and \code{\link[lattice]{levelplot}} from package \CRANpkg{lattice}.
}
\examples{
require(grDevices) # for colours
filled.contour(volcano, color = terrain.colors, asp = 1) # simple

x <- 10*1:nrow(volcano)
y <- 10*1:ncol(volcano)
filled.contour(x, y, volcano, color = terrain.colors,
    plot.title = title(main = "The Topography of Maunga Whau",
    xlab = "Meters North", ylab = "Meters West"),
    plot.axes = { axis(1, seq(100, 800, by = 100))
                  axis(2, seq(100, 600, by = 100)) },
    key.title = title(main = "Height\n(meters)"),
    key.axes = axis(4, seq(90, 190, by = 10)))  # maybe also asp = 1
mtext(paste("filled.contour(.) from", R.version.string),
      side = 1, line = 4, adj = 1, cex = .66)

# Annotating a filled contour plot
a <- expand.grid(1:20, 1:20)
b <- matrix(a[,1] + a[,2], 20)
filled.contour(x = 1:20, y = 1:20, z = b,
               plot.axes = { axis(1); axis(2); points(10, 10) })

## Persian Rug Art:
x <- y <- seq(-4*pi, 4*pi, len = 27)
r <- sqrt(outer(x^2, y^2, "+"))
filled.contour(cos(r^2)*exp(-r/(2*pi)), axes = FALSE)
## rather, the key *should* be labeled:
filled.contour(cos(r^2)*exp(-r/(2*pi)), frame.plot = FALSE,
               plot.axes = {})
}
\keyword{hplot}
\keyword{aplot}
