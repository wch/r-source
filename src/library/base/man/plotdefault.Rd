\name{plot.default}
\title{The Default Scatterplot Function}
\usage{
plot.default(x, y=NULL, type = "p", col = par("fg"), bg = NA,
             pch = par("pch"), xlim = NULL, ylim = NULL, log = "",
             axes = TRUE, frame.plot = axes, panel.first = NULL,
             panel.last = NULL, ann = par("ann"), main = NULL,
             sub = NULL, xlab = NULL, ylab = NULL, cex = par("cex"),
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
  \item{sub}{a sub title for the plot.}
  \item{xlab}{a label for the x axis.}
  \item{ylab}{a label for the y axis.}
  \item{cex}{a numerical value giving the amount by which plotting text
    and symbols should be scaled relative to the default}
  \item{lty}{the line type, see \code{\link{par}.}
  \item{lwd}{the line width \bold{not yet supported for postscript}.}
  \item{\dots}{graphical parameters as in \code{\link{par}(\dots)} may
    also be passed as arguments.  Additionally, \code{asp}, the
    \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.}
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
  \code{\link{plot}}, \code{\link{plot.window}}, \code{\link{xy.coords}}.
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

##--- Log-Log Plot  with  custom axes
lx <- seq(1,5, length=41)
yl <- expression(e^{-frac(1,2) * {log[10](x)}^2})
y <- exp(-.5*lx^2)
op <- par(mfrow=c(2,1), mar=par("mar")+c(0,1,0,0))
plot(10^lx, y, log="xy", type="l", col="purple",
     main="Log-Log plot", ylab=yl, xlab="x")
plot(10^lx, y, log="xy", type="o", pch='.', col="forestgreen",
     main="Log-Log plot with custom axes", ylab=yl, xlab="x",
     axes = FALSE, frame.plot = TRUE)
axis(1, at = my.at <- 10^(1:5), labels = formatC(my.at, format="fg"))
at.y <- 10^(-5:-1)
axis(2, at = at.y, labels = formatC(at.y, format="fg"), col.axis="red")
par(op)
\keyword{hplot}
