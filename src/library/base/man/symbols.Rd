\name{symbols}
\alias{symbols}
\title{Draw symbols on a plot}
\description{
  This function draws symbols on a plot.  One of six symbols;
  \emph{circles}, \emph{squares}, \emph{rectangles}, \emph{stars},
  \emph{thermometers}, and \emph{boxplots}, can be plotted at a
  specified set of x and y coordinates.  Specific aspects of the
  symbols, such as relative size, can be customized by additional
  parameters.
}
\usage{
symbols(x, y, circles, squares, rectangles, stars,
        thermometers, boxplots, inches=TRUE, add=FALSE,
        fg=1, bg=NA, xlab = "", ylab = "", xlim = NULL,
        ylim = NULL, \dots)
}
\arguments{
  \item{x}{a vector giving the x coordinates of the symbols.}
  \item{y}{a vector giving the y coordinates of the symbols.}
  \item{circles}{a vector giving the radii of the circles.}
  \item{squares}{a vector giving the length of the sides of the squares.}
  \item{rectangles}{a matrix with two columns.  The first column gives
        widths and the second the heights of rectangle symbols.}
  \item{stars}{a matrix with three or more columns giving the lengths
       of the rays from the center of the stars.  \code{NA} values
       are replaced by zeroes.}
  \item{thermometers}{a matrix with three or four columns.  The first
        two columns give the width and height of the thermometer symbols.
        If there are three columns, the third is taken as a proportion.
        The thermometers are filled from their base to this proportion
        of their height.  If there are four columns, the third and fourth
        columns are taken as proportions.  The thermometers are filled
        between these two proportions of their heights.}
  \item{boxplots}{a matrix with five columns.  The first two columns
        give the width and height of the boxes and the third the
        proportion of the way up the box that the median line is
	drawn.  The last two columns give the lengths of the lower and
        upper whiskers.}
  \item{inches}{If \code{inches} is \code{FALSE}, the units are taken
        to be those of the x axis.  If \code{inches} is \code{TRUE},
        the symbols are scaled so that the largest symbol is one
        inch in height.  If a number is given the symbols are scaled to
	make largest symbol this height in inches.}
  \item{add}{if \code{add} is \code{TRUE}, the symbols are added to
        an existing plot, otherwise a new plot is created. If a new plot
	is created the axis labels default to \code{""}.}
  \item{fg}{colors the symbols are to be drawn in (the default is
        the value of the \code{col} graphics parameter).}
  \item{bg}{if specified, the symbols are filled with this color.
    The default is to leave the symbols unfilled. }
  \item{xlab}{the x label of the plot.}  
  \item{ylab}{the y label of the plot.}
  \item{xlim}{numeric of length 2 giving the x limits for the plot.}
  \item{ylim}{numeric of length 2 giving the y limits for the plot.}
  \item{\dots}{graphics parameters can also be passed to this function.}
}
\details{
  Observations which have missing coordinates or missing size
  parameters are not plotted.  The exception to this is \emph{stars}.
  In that case, the length of any rays which are \code{NA} is reset
  to zero.
}
\references{
  W. S. Cleveland (1985)
  \emph{The Elements of Graphing Data.}
  Monterey, California: Wadsworth.
}
\examples{
x <- 1:10
y <- runif(10)
z <- runif(10)
symbols(x, y, thermometers=cbind(.5, 1, z), inches=.5)

data(trees)
attach(trees)
## Girth is diameter in inches
symbols(Height, Volume, circles=Girth/24, inches=FALSE,
        xlab="Height", ylab="Volume")
detach()
}
\keyword{aplot}
\keyword{hplot}
