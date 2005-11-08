\name{plot.default}
\alias{plot.default}
\title{The Default Scatterplot Function}
\description{
  Draw a scatter plot with \dQuote{decorations} such as axes and titles
  in the active graphics window.
}
\usage{
\method{plot}{default}(x, y = NULL, type = "p",  xlim = NULL, ylim = NULL,
     log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL,
     ann = par("ann"), axes = TRUE, frame.plot = axes,
     panel.first = NULL, panel.last = NULL, asp = NA, \dots)
}
\arguments{
  \item{x, y}{the \code{x} and \code{y} arguments provide the x and y
    coordinates for the plot.  Any reasonable way of defining the
    coordinates is acceptable.  See the function \code{\link{xy.coords}}
    for details.}
  \item{type}{1-character string giving the type of plot desired.  The
    following values are possible, for details, see \code{\link{plot}}:
    \code{"p"} for points, \code{"l"} for lines,
    \code{"o"} for overplotted points and lines,
    \code{"b"}, \code{"c"}) for (empty if \code{"c"}) points joined by lines,
    \code{"s"} and \code{"S"} for stair steps and
    \code{"h"} for histogram-like vertical lines.  Finally,
    \code{"n"} does not produce any points or lines.}
  \item{xlim}{the x limits (min,max) of the plot.}
  \item{ylim}{the y limits of the plot.}
  \item{log}{a character string which contains \code{"x"} if the x axis
    is to be logarithmic, \code{"y"} if the y axis is to be logarithmic
    and \code{"xy"} or \code{"yx"} if both axes are to be logarithmic.}

  \item{main}{a main title for the plot.}
  \item{sub}{a sub title for the plot.}
  \item{xlab}{a label for the x axis, defaults to a description of \code{x}.}
  \item{ylab}{a label for the y axis, defaults to a description of \code{y}.}

  \item{ann}{a logical value indicating whether the default annotation
    (title and x and y axis labels) should appear on the plot.}
  \item{axes}{a logical value indicating whether axes should be drawn on
    the plot.}
  \item{frame.plot}{a logical indicating whether a box should be drawn
    around the plot.}

  \item{panel.first}{an expression to be evaluated after the plot axes
    are set up but before any plotting takes place.  This can be useful
    for drawing background grids or scatterplot smooths.}
  \item{panel.last}{an expression to be evaluated after plotting has
    taken place.}

  \item{asp}{the \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.}
  \item{\dots}{other graphical parameters (see\code{\link{par}}).}
}
\details{
  Commonly used graphical parameters are:
  \describe{
    \item{\code{col}}{The colors for lines and points.  Multiple colors can be
      specified so that each point can be given its own color.  If there
      are fewer colors than points they are recycled in the standard
      fashion.  Lines will all be plotted in the first colour specified.}
    \item{\code{bg}}{a vector of background colors for open plot symbols, see
      \code{\link{points}}.  Note: this is \bold{not} the same setting
      as \code{\link{par}("bg")}.}
    \item{\code{pch}}{a vector of plotting characters or symbols:
      see \code{\link{points}}.}
    \item{\code{cex}}{a numerical vector giving the amount by which
      plotting text and symbols should be scaled relative to the
      default.  This works as a multiple of \code{\link{par}("cex")}.
      \code{NULL} and \code{NA} are equivalent to \code{1.0}.}
    \item{\code{lty}}{the line type, see \code{\link{par}}.}
    \item{\code{lwd}}{the line width, see \code{\link{par}}.}
  }
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.

  Cleveland, W. S. (1985)
  \emph{The Elements of Graphing Data.}
  Monterey, CA: Wadsworth.

  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}
\seealso{
  \code{\link{plot}}, \code{\link{plot.window}}, \code{\link{xy.coords}}.
}
\examples{
Speed <- cars$speed
Distance <- cars$dist
plot(Speed, Distance, panel.first = grid(8,8),
     pch = 0, cex = 1.2, col = "blue")
plot(Speed, Distance,
     panel.first = lines(lowess(Speed, Distance), lty = "dashed"),
     pch = 0, cex = 1.2, col = "blue")

## Show the different plot types
x <- 0:12
y <- sin(pi/5 * x)
op <- par(mfrow = c(3,3), mar = .1+ c(2,2,3,1))
for (tp in c("p","l","b",  "c","o","h",  "s","S","n")) {
   plot(y ~ x, type = tp,
        main = paste("plot(*, type = \"",tp,"\")",sep=""))
   if(tp == "S") {
      lines(x,y, type = "s", col = "red", lty = 2)
      mtext("lines(*, type = \"s\", ...)", col = "red", cex=.8)
   }
}
par(op)

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
}
\keyword{hplot}
