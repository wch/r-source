% File src/library/graphics/man/plotdefault.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2012 R Core Team
% Distributed under GPL 2 or later

\name{plot.default}
\alias{plot.default}
\title{The Default Scatterplot Function}
\description{
  Draw a scatter plot with decorations such as axes and titles
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
    for details.  If supplied separately, they must be of the same length.}
  \item{type}{1-character string giving the type of plot desired.  The
    following values are possible, for details, see \code{\link{plot}}:
    \code{"p"} for points, \code{"l"} for lines,
    \code{"b"} for both points and lines,
    \code{"c"} for empty points joined by lines,
    \code{"o"} for overplotted points and lines,
    \code{"s"} and \code{"S"} for stair steps and
    \code{"h"} for histogram-like vertical lines.  Finally,
    \code{"n"} does not produce any points or lines.}
  \item{xlim}{the x limits (x1, x2) of the plot.  Note that \code{x1 > x2}
    is allowed and leads to a \sQuote{reversed axis}.

    The default value, \code{NULL}, indicates that the range of the
    \link{finite} values to be plotted should be used.}
  \item{ylim}{the y limits of the plot.}
  \item{log}{a character string which contains \code{"x"} if the x axis
    is to be logarithmic, \code{"y"} if the y axis is to be logarithmic
    and \code{"xy"} or \code{"yx"} if both axes are to be logarithmic.}

  \item{main}{a main title for the plot, see also \code{\link{title}}.}
  \item{sub}{a sub title for the plot.}
  \item{xlab}{a label for the x axis, defaults to a description of \code{x}.}
  \item{ylab}{a label for the y axis, defaults to a description of \code{y}.}

  \item{ann}{a logical value indicating whether the default annotation
    (title and x and y axis labels) should appear on the plot.}
  \item{axes}{a logical value indicating whether both axes should be drawn on
    the plot.  Use \link{graphical parameter} \code{"xaxt"} or \code{"yaxt"}
    to suppress just one of the axes.}
  \item{frame.plot}{a logical indicating whether a box should be drawn
    around the plot.}

  \item{panel.first}{an \sQuote{expression} to be evaluated after the
    plot axes are set up but before any plotting takes place.  This can
    be useful for drawing background grids or scatterplot smooths.  Note
    that this works by lazy evaluation: passing this argument from other
    \code{plot} methods may well not work since it may be evaluated too
    early.}

  \item{panel.last}{an expression to be evaluated after plotting has
    taken place but before the axes, title and box are added.  See the
    comments about \code{panel.first}.}

  \item{asp}{the \eqn{y/x} aspect ratio, see \code{\link{plot.window}}.}
  \item{\dots}{other \link{graphical parameters} (see \code{\link{par}} and
    section \sQuote{Details} below).}
}
\details{
  Commonly used \link{graphical parameters} are:
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
      plotting characters and symbols should be scaled relative to the
      default.  This works as a multiple of \code{\link{par}("cex")}.
      \code{NULL} and \code{NA} are equivalent to \code{1.0}.  Note that
      this does not affect annotation: see below.}
    \item{\code{lty}}{a vector of line types, see \code{\link{par}}.}
    \item{\code{cex.main}, \code{col.lab}, \code{font.sub},
      etc}{settings for main- and sub-title and axis annotation, see
      \code{\link{title}} and \code{\link{par}}.}
    \item{\code{lwd}}{a vector of line widths, see \code{\link{par}}.}
  }
}
\note{
  The presence of \code{panel.first} and \code{panel.last} is a
  historical anomaly: default plots do not have \sQuote{panels}, unlike
  e.g.\sspace{}\code{\link{pairs}} plots.  For more control, use lower-level
  plotting functions: \code{plot.default} calls in turn some of
  \code{\link{plot.new}}, \code{\link{plot.window}},
  \code{\link{plot.xy}}, \code{\link{axis}}, \code{\link{box}} and
  \code{\link{title}}, and plots can be built up by calling these
  individually, or by calling \code{plot(type = "n")} and adding further
  elements.
}

\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

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
plot(Speed, Distance, panel.first = grid(8, 8),
     pch = 0, cex = 1.2, col = "blue")
plot(Speed, Distance,
     panel.first = lines(stats::lowess(Speed, Distance), lty = "dashed"),
     pch = 0, cex = 1.2, col = "blue")

## Show the different plot types
x <- 0:12
y <- sin(pi/5 * x)
op <- par(mfrow = c(3,3), mar = .1+ c(2,2,3,1))
for (tp in c("p","l","b",  "c","o","h",  "s","S","n")) {
   plot(y ~ x, type = tp, main = paste0("plot(*, type = \"", tp, "\")"))
   if(tp == "S") {
      lines(x, y, type = "s", col = "red", lty = 2)
      mtext("lines(*, type = \"s\", ...)", col = "red", cex = 0.8)
   }
}
par(op)

##--- Log-Log Plot  with  custom axes
lx <- seq(1, 5, length = 41)
yl <- expression(e^{-frac(1,2) * {log[10](x)}^2})
y <- exp(-.5*lx^2)
op <- par(mfrow = c(2,1), mar = par("mar")+c(0,1,0,0))
plot(10^lx, y, log = "xy", type = "l", col = "purple",
     main = "Log-Log plot", ylab = yl, xlab = "x")
plot(10^lx, y, log = "xy", type = "o", pch = ".", col = "forestgreen",
     main = "Log-Log plot with custom axes", ylab = yl, xlab = "x",
     axes = FALSE, frame.plot = TRUE)
my.at <- 10^(1:5)
axis(1, at = my.at, labels = formatC(my.at, format = "fg"))
at.y <- 10^(-5:-1)
axis(2, at = at.y, labels = formatC(at.y, format = "fg"), col.axis = "red")
par(op)
}
\keyword{hplot}
