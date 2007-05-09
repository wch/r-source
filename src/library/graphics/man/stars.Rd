\name{stars}
\alias{stars}
\title{Star (Spider/Radar) Plots and Segment Diagrams}
\description{
  Draw star plots or segment diagrams of a multivariate data set.
  With one single location, also draws \dQuote{spider} (or \dQuote{radar}) plots.
}
\usage{
stars(x, full = TRUE, scale = TRUE, radius = TRUE,
      labels = dimnames(x)[[1]], locations = NULL,
      nrow = NULL, ncol = NULL, len = 1,
      key.loc = NULL, key.labels = dimnames(x)[[2]], key.xpd = TRUE,
      xlim = NULL, ylim = NULL, flip.labels = NULL,
      draw.segments = FALSE, col.segments = 1:n.seg, col.stars = NA,
      axes = FALSE, frame.plot = axes,
      main = NULL, sub = NULL, xlab = "", ylab = "",
      cex = 0.8, lwd = 0.25, lty = par("lty"), xpd = FALSE,
      mar = pmin(par("mar"),
                 1.1+ c(2*axes+ (xlab != ""),
                 2*axes+ (ylab != ""), 1,0)),
      add = FALSE, plot = TRUE, \dots)
}
\arguments{
  \item{x}{matrix or data frame of data.
    One star or segment plot will be produced for each row of \code{x}.
    Missing values (\code{NA}) are allowed, but they are treated as if
    they were 0 (after scaling, if relevant).}
  \item{full}{logical flag: if \code{TRUE}, the segment plots will
    occupy a full circle.  Otherwise, they occupy the (upper) semicircle
    only.}
  \item{scale}{logical flag: if \code{TRUE}, the columns of the data
    matrix are scaled independently so that the maximum value in each
    column is 1 and the minimum is 0.  If \code{FALSE}, the presumption
    is that the data have been scaled by some other algorithm to the
    range \eqn{[0,1]}.}
  \item{radius}{logical flag: in \code{TRUE}, the radii corresponding to
    each variable in the data will be drawn.}
  \item{labels}{vector of character strings for labeling the plots.
    Unlike the S function \code{stars}, no attempt is made to construct
    labels if \code{labels = NULL}.}
  \item{locations}{Either two column matrix with the x and y coordinates used
    to place each of the segment plots; or numeric of length 2 when all
    plots should be superimposed (for a \dQuote{spider plot}).
    By default, \code{locations = NULL}, the segment plots will be
    placed in a rectangular grid.}
  \item{nrow, ncol}{integers giving the number of rows and columns to use when
    \code{locations} is \code{NULL}.  By default, \code{nrow == ncol}, a square
    layout will be used.}
  \item{len}{scale factor for the length of radii or segments.}
  \item{key.loc}{vector with x and y coordinates of the unit key.}
  \item{key.labels}{vector of character strings for labeling the
    segments of the unit key.  If omitted, the second component of
    \code{dimnames(x)} is used, if available.}
  \item{key.xpd}{clipping switch for the unit key (drawing and
    labeling), see \code{\link{par}("xpd")}.}
  \item{xlim}{vector with the range of x coordinates to plot. }
  \item{ylim}{vector with the range of y coordinates to plot.}
  \item{flip.labels}{logical indicating if the label locations should
    flip up and down from diagram to diagram. Defaults to a somewhat
    smart heuristic.}
  \item{draw.segments}{logical. If \code{TRUE} draw a segment diagram.}
  \item{col.segments}{color vector (integer or character, see
    \code{\link{par}}), each specifying a color for one of the segments
    (variables).  Ignored if \code{draw.segments = FALSE}.}
  \item{col.stars}{color vector (integer or character, see
    \code{\link{par}}), each specifying a color for one of the stars
    (cases).  Ignored if \code{draw.segments = TRUE}.}
  \item{axes}{logical flag: if \code{TRUE} axes are added to the plot.}
  \item{frame.plot}{logical flag: if \code{TRUE}, the plot region is framed.}
  \item{main}{a main title for the plot.}
  \item{sub}{a sub title for the plot.}
  \item{xlab}{a label for the x axis.}
  \item{ylab}{a label for the y axis.}
  \item{cex}{character expansion factor for the labels.}
  \item{lwd}{line width used for drawing.}
  \item{lty}{line type used for drawing.}
  \item{xpd}{logical or NA indicating if clipping should be done, see
    \code{\link{par}(xpd = .)}.}
  \item{mar}{argument to \code{\link{par}(mar = *)}, typically choosing
    smaller margins than by default.}
  \item{\dots}{further arguments, passed to the first call of
    \code{plot()}, see \code{\link{plot.default}} and to
    \code{\link{box}()} if \code{frame.plot} is true.}
  \item{add}{logical, if \code{TRUE} \emph{add} stars to current plot.}
  \item{plot}{logical, if \code{FALSE}, nothing is plotted.}
}
\details{
  Missing values are treated as 0.

  Each star plot or segment diagram represents one row of the input
  \code{x}.  Variables (columns) start on the right and wind
  counterclockwise around the circle.  The size of the (scaled) column
  is shown by the distance from the center to the point on the star or
  the radius of the segment representing the variable.

  Only one page of output is produced.
}
\author{Thomas S. Dye}
\seealso{
  \code{\link{symbols}} for another way to draw stars and other symbols.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\note{
  This code started life as spatial star plots by David A. Andrews.
  See \url{http://www.udallas.edu:8080/~andrews/software/software.html}.

  Prior to 1.4.1, scaling only shifted the maximum to 1, although
  documented as here.
}
\examples{
require(grDevices)
stars(mtcars[, 1:7], key.loc = c(14, 2),
      main = "Motor Trend Cars : stars(*, full = F)", full = FALSE)
stars(mtcars[, 1:7], key.loc = c(14, 1.5),
      main = "Motor Trend Cars : full stars()",flip.labels=FALSE)

## 'Spider' or 'Radar' plot:
stars(mtcars[, 1:7], locations = c(0,0), radius = FALSE,
      key.loc=c(0,0), main="Motor Trend Cars", lty = 2)

## Segment Diagrams:
palette(rainbow(12, s = 0.6, v = 0.75))
stars(mtcars[, 1:7], len = 0.8, key.loc = c(12, 1.5),
      main = "Motor Trend Cars", draw.segments = TRUE)
stars(mtcars[, 1:7], len = 0.6, key.loc = c(1.5, 0),
      main = "Motor Trend Cars", draw.segments = TRUE,
      frame.plot=TRUE, nrow = 4, cex = .7)

## scale linearly (not affinely) to [0, 1]
USJudge <- apply(USJudgeRatings, 2, function(x) x/max(x))
Jnam <- row.names(USJudgeRatings)
Snam <- abbreviate(substring(Jnam,1,regexpr("[,.]",Jnam) - 1), 7)
stars(USJudge, labels = Jnam, scale = FALSE,
      key.loc = c(13, 1.5), main = "Judge not ...", len = 0.8)
stars(USJudge, labels = Snam, scale = FALSE,
      key.loc = c(13, 1.5), radius = FALSE)

loc <- stars(USJudge, labels = NULL, scale = FALSE,
             radius = FALSE, frame.plot = TRUE,
             key.loc = c(13, 1.5), main = "Judge not ...", len = 1.2)
text(loc, Snam, col = "blue", cex = 0.8, xpd = TRUE)

## 'Segments':
stars(USJudge, draw.segments = TRUE, scale = FALSE, key.loc = c(13,1.5))

## 'Spider':
stars(USJudgeRatings, locations=c(0,0), scale=FALSE,radius = FALSE,
      col.stars=1:10, key.loc = c(0,0), main="US Judges rated")
## 'Radar-Segments'
stars(USJudgeRatings[1:10,], locations = 0:1, scale=FALSE,
      draw.segments = TRUE, col.segments=0, col.stars=1:10,key.loc= 0:1,
       main="US Judges 1-10 ")
palette("default")
stars(cbind(1:16,10*(16:1)),draw.segments=TRUE,
      main = "A Joke -- do *not* use symbols on 2D data!")
}
\keyword{hplot}
\keyword{multivariate}
