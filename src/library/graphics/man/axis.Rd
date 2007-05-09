\name{axis}
\alias{axis}
\title{Add an Axis to a Plot}
\description{Adds an axis to the current plot, allowing the
  specification of the side, position, labels, and other options.
}
\usage{
axis(side, at = NULL, labels = TRUE, tick = TRUE, line = NA,
     pos = NA, outer = FALSE, font = NA,
     lty = "solid", lwd = 1, col = NULL, hadj = NA, padj = NA,
     \dots)
}
\arguments{
  \item{side}{an integer specifying which side of the plot the axis is
    to be drawn on.  The axis is placed as follows: 1=below,
    2=left, 3=above and 4=right.}
  \item{at}{the points at which tick-marks are to be drawn.  Non-finite
    (infinite, \code{NaN} or \code{NA}) values are omitted.  By default,
    when \code{NULL}, tickmark locations are computed, see Details below.}
  \item{labels}{this can either be a logical value specifying whether
    (numerical) annotations are to be made at the tickmarks, or a
    character or expression vector of labels to be placed at the
    tickpoints.  (Other objects are coerced by \code{\link{as.graphicsAnnot}}.)
    If this is not logical, \code{at} should also be supplied and of the
    same length.  If \code{labels} is of length zero after coercion,
    it has the same effect as supplying \code{TRUE}.}
  \item{tick}{a logical value specifying whether tickmarks and an axis line
    should be drawn}.
  \item{line}{the number of lines into the margin which the axis will
    be drawn.  If not \code{NA} this overrides the value of the
    graphical parameter \code{mgp[3]}.  The relative placing of
    tickmarks and tick labels is unchanged.}
  \item{pos}{the coordinate at which the axis line is to be drawn:
    if not \code{NA} this overrides the values of both \code{line} and
    \code{mgp[3]}.}
  \item{outer}{a logical value indicating whether the axis should be
    drawn in the outer plot margin, rather than the standard plot
    margin.}
  \item{font}{font for text.  Defaults to \code{par("font")}.}
  \item{lty, lwd}{line type, width for the axis line and the tick marks.}
  \item{col}{color for the axis line and the tick marks.  Here
    \code{NULL} means to use \code{par("fg")}, possibly specified inline.}
  \item{hadj}{adjustment (see \code{\link{par}("adj")}) for all labels
    \emph{parallel} (\sQuote{horizontal}) to the reading direction.  If
    this is not a finite value, the default is used (centring for
    strings parallel to the axis, justification of the end nearest the
    axis otherwise).}
  \item{padj}{adjustment for each tick label \emph{perpendicular} to the
    reading direction.  For labels parallel to the axes, \code{padj=0}
    means right or top alignment, and \code{padj=1} means left or bottom
    alignment.  This can be a vector given a value for each string, and
    will be recycled as necessary.
    
    If \code{padj} is not a finite value (the default), the value of
    \code{par("las")} determines the adjustment.  For strings plotted
    perpendicular to the axis the default is to centre the string.}

  \item{\dots}{other graphical parameters may also be passed as
    arguments to this function, particularly, \code{cex.axis},
    \code{col.axis} and \code{font.axis} for axis annotation, \code{mgp}
    and \code{xaxp} or \code{yaxp} for positioning, \code{tck} or
    \code{tcl} for tick mark length and direction, \code{las} for
    vertical/horizontal label orientation, or \code{fg} instead of
    \code{col}, see \code{\link{par}} on these.

    Parameters \code{xaxt} (sides 1 and 3) and \code{yaxt} (sides 2 and
    4) control if the axis is plotted at all.

    Note that \code{xpd} is not accepted as clipping is always to the
    device region, and that \code{lab} will partial match to argument
    \code{labels} unless the latter is also supplied.  (Since the
    default axes has already been set up by \code{\link{plot.window}},
    \code{lab} will not be acted on by \code{axis}.)}
}
\value{
  The numeric locations on the axis scale at which tick marks were drawn
  when the plot was first drawn (see Details).
  
  This function is usually invoked for its side effect, which is to add
  an axis to an already existing plot.
}
\details{
  The axis line is drawn from the lowest to the highest value of
  \code{at}, but will be clipped at the plot region.  Only ticks which
  are drawn from points within the plot region (up to a tolerance for
  rounding error) are plotted, but the ticks and their labels may well
  extend outside the plot region.

  When \code{at = NULL}, pretty tick mark locations are computed internally
  (the same way \code{\link{axTicks}(side)} would) from
  \code{\link{par}("xaxp")} or \code{"yaxp"} and
  \code{\link{par}("xlog")} (or \code{"ylog"}).  Note that these
  locations may change if an on-screen plot is resized (for example, if
  the \code{plot} argument \code{asp} (see \code{\link{plot.window}}) is set.)

  If \code{labels} is not specified, the numeric values supplied or
  calculated for \code{at} are converted to character strings as if they
  were a numeric vector printed by \code{\link{print.default}(digits=7)}.
  
  The code tries hard not to draw overlapping tick labels, and so will
  omit labels where they would abut or overlap previously drawn labels.
  This can result in, for example, every other tick being labelled.
  (The ticks are drawn left to right or bottom to top, and space at
  least the size of an \sQuote{m} is left between labels.)

  Several of the graphics parameters affect the way axes are drawn. The
  vertical (for sides 1 and 3) positions of the axis and the tick labels
  are controlled by \code{mgp}, the size and direction of the ticks is
  controlled by \code{tck} and \code{tcl} and the appearance of the tick
  labels by \code{cex.axis}, \code{col.axis} and \code{font.axis} with
  orientation controlled by \code{las} (but not \code{srt}, unlike S
  which uses \code{srt} if \code{at} is supplied and \code{las} if it is
  not).  Note that \code{adj} is not supported.
  See \code{\link{par}} for details.
}
\seealso{
  \code{\link{Axis}} for a generic interface.
  
  \code{\link{axTicks}} returns the axis tick locations
  corresponding to \code{at=NULL}; \code{\link{pretty}} is more flexible
  for computing pretty tick coordinates and does \emph{not} depend on
  (nor adapt to) the coordinate system in use.  
  
  Several graphics parameters affecting the appearance are documented 
  in \code{\link{par}}.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
require(stats) # for rnorm
plot(1:4, rnorm(4), axes = FALSE)
axis(1, 1:4, LETTERS[1:4])
axis(2)
box() #- to make it look "as usual"

plot(1:7, rnorm(7), main = "axis() examples",
     type = "s", xaxt = "n", frame = FALSE, col = "red")
axis(1, 1:7, LETTERS[1:7], col.axis = "blue")
# unusual options:
axis(4, col = "violet", col.axis="dark violet", lwd = 2)
axis(3, col = "gold", lty = 2, lwd = 0.5)

# one way to have a custom x axis
plot(1:10, xaxt = "n")
axis(1, xaxp=c(2, 9, 7))
}
\keyword{aplot}
