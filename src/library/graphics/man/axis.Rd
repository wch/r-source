\name{axis}
\alias{axis}
\title{Add an Axis to a Plot}
\description{Adds an axis to the current plot, allowing the
  specification of the side, position, labels, and other options.
}
\usage{
axis(side, at = NULL, labels = TRUE, tick = TRUE, line = NA,
     pos = NA, outer = FALSE, font = NA, vfont = NULL,
     lty = "solid", lwd = 1, col = NULL, padj = NA, \dots)
}
\arguments{
  \item{side}{an integer specifying which side of the plot the axis is
    to be drawn on.  The axis is placed as follows: 1=below,
  2=left, 3=above and 4=right.}
  \item{at}{the points at which tick-marks are to be drawn.  Non-finite
    (infinite, \code{NaN} or \code{NA}) values are omitted.  By default,
    when \code{NULL}, tickmark locations are computed, see Details below.}
  \item{labels}{this can either be a logical value specifying whether
    (numerical) annotations are to be made at the tickmarks, or a vector
    of character strings to be placed at the tickpoints.}
  \item{tick}{a logical value specifying whether tickmarks should be
    drawn}.
  \item{line}{the number of lines into the margin which the axis will
    be drawn.  This overrides the value of the graphical parameter
    \code{mgp[3]}.  The relative placing of tickmarks and tick labels
    is unchanged.}
  \item{pos}{the coordinate at which the axis line is to be drawn:
    this overrides the values of both \code{line} and \code{mgp[3]}.}
  \item{outer}{a logical value indicating whether the axis should be
    drawn in the outer plot margin, rather than the standard plot
    margin.}
  \item{font}{font for text.}
  \item{vfont}{vector font for text.}
  \item{lty, lwd}{line type, width for the axis line and the tick marks.}
  \item{col}{color for the axis line and the tick marks.  The default
    \code{NULL} means to use \code{par("fg")}.}
  \item{padj}{adjustment for each string perpendicular to the reading direction.
    For strings parallel to the axes, \code{padj=0} means bottom or right alignment, 
    and \code{padj=1} means top or left alignment.
    If \code{padj} is not a finite value (the default), the value of
    \code{par("las")} determines the adjustment.  For strings plotted
    perpendicular to the axis the default is to centre the string.}
  \item{\dots}{other graphical parameters may also be passed as arguments to
    this function, particularly, \code{cex.axis}, \code{col.axis} and
    \code{font.axis} for axis annotation, \code{las} for
    vertical/horizontal label orientation, or \code{fg} instead of
    \code{col}, see \code{\link{par}} on these.}
}
\value{
  This function is invoked for its side effect, which is to add an axis
  to an already existing plot.
}
\details{
  The axis line is drawn from the lowest to the highest value of
  \code{at}, but will be clipped at the plot region. Only ticks which
  are drawn from points within the plot region (up to a tolerance for
  rounding error) are plotted, but the ticks and their labels may well
  extend outside the plot region.

  When \code{at = NULL}, pretty tick mark locations are computed internally
  (the same way \code{\link{axTicks}(side)} would) from
  \code{\link{par}("usr", "lab")} and \code{\link{par}("xlog")} (or
  \code{"ylog"}).

  Several of the graphics parameters affect the way axes are drawn. The
  vertical (for sides 1 and 3) positions of the axis and the tick labels
  are controlled by \code{mgp}, the size of the ticks is controlled by
  \code{tck} and \code{tcl} and the appearance of the tick labels by
  \code{cex.axis}, \code{col.axis} and \code{font.axis} with
  orientation controlled by \code{las} (but not \code{srt}, unlike S
  which uses \code{srt} if \code{at} is supplied and \code{las} if it is
  not).
}
\seealso{
  \code{\link{axTicks}} returns the axis tick locations
  corresponding to \code{at=NULL}; \code{\link{pretty}} is more flexible
  for computing pretty tick coordinates and does \emph{not} depend on
  (nor adapt to) the coordinate system in use.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
plot(1:4, rnorm(4), axes=FALSE)
axis(1, 1:4, LETTERS[1:4])
axis(2)
box() #- to make it look "as usual"

plot(1:7, rnorm(7), main = "axis() examples",
     type = "s", xaxt = "n", frame = FALSE, col = "red")
axis(1, 1:7, LETTERS[1:7], col.axis = "blue")
# unusual options:
axis(4, col = "violet", col.axis="dark violet", lwd = 2)
axis(3, col = "gold", lty = 2, lwd = 0.5)
}
\keyword{aplot}
