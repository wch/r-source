% File src/library/graphics/man/symbols.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{symbols}
\alias{symbols}
\concept{circle}
\concept{star}
\concept{square}
\concept{rectangle}
\concept{thermometer}
\concept{boxplot}
\title{Draw Symbols (Circles, Squares, Stars, Thermometers, Boxplots)}
\description{
  This function draws symbols on a plot.  One of six symbols;
  \emph{circles}, \emph{squares}, \emph{rectangles}, \emph{stars},
  \emph{thermometers}, and \emph{boxplots}, can be plotted at a
  specified set of x and y coordinates.  Specific aspects of the
  symbols, such as relative size, can be customized by additional
  parameters.
}
\usage{
symbols(x, y = NULL, circles, squares, rectangles, stars,
        thermometers, boxplots, inches = TRUE, add = FALSE,
        fg = par("col"), bg = NA,
        xlab = NULL, ylab = NULL, main = NULL,
        xlim = NULL, ylim = NULL, \dots)
}
\arguments{
  \item{x, y}{the x and y co-ordinates for the centres of the symbols.
    They can be specified in any way which is accepted by
    \code{\link{xy.coords}}.}
  \item{circles}{a vector giving the radii of the circles.}
  \item{squares}{a vector giving the length of the sides of the squares.}
  \item{rectangles}{a matrix with two columns.  The first column gives
    widths and the second the heights of rectangles.}
  \item{stars}{a matrix with three or more columns giving the lengths
    of the rays from the center of the stars.  \code{NA} values
    are replaced by zeroes.}
  \item{thermometers}{a matrix with three or four columns.  The first
    two columns give the width and height of the thermometer symbols. If
    there are three columns, the third is taken as a proportion: the
    thermometers are filled (using colour \code{fg}) from their base to
    this proportion of their height.  If there are four columns, the
    third and fourth columns are taken as proportions and the
    thermometers are filled between these two proportions of their
    heights.  The part of the box not filled in \code{fg} will be filled
    in the background colour (default transparent) given by \code{bg}.}
  \item{boxplots}{a matrix with five columns.  The first two columns
    give the width and height of the boxes, the next two columns
    give the lengths of the lower and upper whiskers and the fifth
    the proportion (with a warning if not in [0,1]) of the way up
    the box that the median line is drawn.}
  \item{inches}{\code{TRUE}, \code{FALSE} or a positive number.
    See \sQuote{Details}.}
  \item{add}{if \code{add} is \code{TRUE}, the symbols are added to
    an existing plot, otherwise a new plot is created.}
  \item{fg}{colour(s) the symbols are to be drawn in.}
  \item{bg}{if specified, the symbols are filled with colour(s), the
    vector \code{bg} being recycled to the number of symbols.
    The default is to leave the symbols unfilled.}
  \item{xlab}{the x label of the plot if \code{add} is not true.
    Defaults to the \code{\link{deparse}}d expression used for \code{x}.}
  \item{ylab}{the y label of the plot.  Unused if \code{add = TRUE}.}
  \item{main}{a main title for the plot.  Unused if \code{add = TRUE}.}
  \item{xlim}{numeric vector of length 2 giving the x limits for the
    plot.  Unused if \code{add = TRUE}.}
  \item{ylim}{numeric vector of length 2 giving the y limits for the
    plot.  Unused if \code{add = TRUE}.}
  \item{\dots}{graphics parameters can also be passed to this function,
    as can the plot aspect ratio \code{asp} (see \code{\link{plot.window}}).}
}
\details{
  Observations which have missing coordinates or missing size
  parameters are not plotted.  The exception to this is \emph{stars}.
  In that case, the length of any ray which is \code{NA} is reset
  to zero.

  Argument \code{inches} controls the sizes of the symbols.  If
  \code{TRUE} (the default), the symbols are scaled so that the
  largest dimension of any symbol is one inch.  If a positive number is
  given the symbols are scaled to make largest dimension this size in
  inches (so \code{TRUE} and \code{1} are equivalent).  If \code{inches}
  is \code{FALSE}, the units are taken to be those of the appropriate
  axes.  (For circles, squares and stars the units of the x axis are
  used.  For boxplots, the lengths of the whiskers are regarded as
  dimensions alongside width and height when scaling by \code{inches},
  and are otherwise interpreted in the units of the y axis.)

  % The next is enforced in GCircle.
  Circles of radius zero are plotted at radius one pixel (which is
  device-dependent).  Circles of a very small non-zero radius may or may
  not be visible, and may be smaller than circles of radius zero.  On
  \code{windows} devices circles are plotted at radius at least one
  pixel as some Windows versions omit smaller circles.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  W. S. Cleveland (1985)
  \emph{The Elements of Graphing Data.}
  Monterey, California: Wadsworth.

  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}
\seealso{
  \code{\link{stars}} for drawing \emph{stars} with a bit more
  flexibility.

  If you are thinking about doing \sQuote{bubble plots} by
  \code{symbols(*, circles=*)}, you should \emph{really} consider using
  \code{\link{sunflowerplot}} instead.
}
\examples{
require(stats); require(grDevices)
x <- 1:10
y <- sort(10*runif(10))
z <- runif(10)
z3 <- cbind(z, 2*runif(10), runif(10))
symbols(x, y, thermometers = cbind(.5, 1, z), inches = .5, fg = 1:10)
symbols(x, y, thermometers = z3, inches = FALSE)
text(x, y, apply(format(round(z3, digits = 2)), 1, paste, collapse = ","),
     adj = c(-.2,0), cex = .75, col = "purple", xpd = NA)

## Note that  example(trees)  shows more sensible plots!
N <- nrow(trees)
with(trees, {
## Girth is diameter in inches
symbols(Height, Volume, circles = Girth/24, inches = FALSE,
        main = "Trees' Girth") # xlab and ylab automatically
## Colours too:
op <- palette(rainbow(N, end = 0.9))
symbols(Height, Volume, circles = Girth/16, inches = FALSE, bg = 1:N,
        fg = "gray30", main = "symbols(*, circles = Girth/16, bg = 1:N)")
palette(op)
})
}
\keyword{aplot}
\keyword{hplot}
\keyword{multivariate}
