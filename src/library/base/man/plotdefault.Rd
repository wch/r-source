\name{plot.default}
\title{The Default Scatterplot Function}
\usage{
plot.default(x, y=NULL, type = "p", col = par("fg"), bg = NA,
             pch = par("pch"), xlim = NULL, ylim = NULL, log = "",
             axes = TRUE, frame.plot = axes, panel.first = NULL,
             panel.last = NULL, ann = par("ann"), main = NULL,
             xlab = NULL, ylab = NULL, cex = par("cex"),
             lty = par("lty"), lwd = par("lwd"), \dots)
}
\alias{plot.default}
\arguments{
  \item{x,y}{the \code{x} and \code{y} arguments provide the x and y
    coordinates for the plot.  Any reasonble way of defining the
    coordinates is acceptable.  See the function \code{\link{xy.coords}}
    for details.}
  \item{type}{the type of plot desired.  The following possibilities are
    possible; \code{"p"} for points, \code{"l"} for lines, \code{"o"}
    for overplotted points and lines, \code{"b"} or \code{c} for points
    joined by lines, \code{"s"} and \code{"S"} for step functions and
    \code{"h"} for \emph{high density} vertical lines.}
  \item{col}{The colors for lines and points.  Multiple colors can be
    specified so that each point can be given its own color.  If there
    are fewer colors than points they are recycled in the standard
    fashion.}
  \item{bg}{background color of ???}
  \item{pch}{a vector of plotting characters or symbols.}
  \item{xlim}{the x limits (min,max) of the plot.}
  \item{ylim}{the y limits of the plot.}
  \item{log}{a character string which contains \code{"x"} if the x axis
    is to be logarithmic, \code{"y"} if the y axis is to be logarithmic
    and \code{"xy"} or \code{"yx"} if both axes are to be logarithmic.}
  \item{axes}{a logical value indicating whether axes should be drawn on
    the plot.}
  \item{frame.plot}{a logical indicating whether a box should be drawn
    around the plot.}
  \item{panel.first}{an expression to be evaluated after the plot axes
    are set up but before any plotting takes place.  This can be useful
    for drawing background grids or scatterplot smooths.}
  \item{panel.last}{an expression to be evaluated after plotting has
    taken place.}
  \item{ann}{an logical value indicating whether the default annotation
    (title and x and y axis labels) should appear on the plot/}
  \item{main}{a main title for the plot.}
  \item{xlab}{a label for the x axis.}
  \item{ylab}{a label for the y axis.}
  \item{cex}{a numerical value giving the amount by which plotting text
    and symbols should be scaled relative to the default}
  \item{lty}{the line type}
  \item{lwd}{the line width (is this used at all ???)}
  \item{\dots}{graphical parameters may also be passed as arguments.}
}
\value{
  This function is invoked for its side effect of drawing a scatter plot
  in the active graphics window.
}
\references{
  Cleveland, W. S. (1985).
  \emph{The Elements of Graphing Data}.
  Monterey, CA: Wadsworth.
}
\seealso{
  \code{\link{plot}}, \code{\link{xy.coords}}.
}
\examples{
data(cars)
Speed <- cars$speed
Distance <- cars$dist
plot(Speed, Distance, panel.first = grid(8,8),
     pch = 0, cex = 1.2, col = "blue")
plot(Speed, Distance,
     panel.first = lines(lowess(Speed, Distance), lty = "dashed"),
     pch = 0, cex = 1.2, col = "blue")
}
\keyword{hplot}
