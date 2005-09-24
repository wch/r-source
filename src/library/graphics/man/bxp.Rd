\name{bxp}
\alias{bxp}
\title{Box Plots from Summaries}
\description{
  \code{bxp} draws box plots based on the given summaries in \code{z}.
  It is usually called from within \code{\link{boxplot}}, but can be
  invoked directly.
}
\usage{
bxp(z, notch = FALSE, width = NULL, varwidth = FALSE, outline = TRUE,
    notch.frac = 0.5, log = "", border = par("fg"), col = par("bg"),
    pars = NULL, frame.plot = axes, horizontal = FALSE,
    add = FALSE, at = NULL, show.names = NULL, \dots)
}
\arguments{
  \item{z}{a list containing data summaries to be used in constructing
    the plots.  These are usually the result of a call to
    \code{\link{boxplot}}, but can be generated in any fashion.}
  \item{notch}{if \code{notch} is \code{TRUE}, a notch is drawn in each
    side of the boxes.  If the notches of two plots do not overlap then
    the medians are significantly different at the 5 percent level.}
  \item{width}{a vector giving the relative widths of the boxes making
    up the plot.}
  \item{varwidth}{if \code{varwidth} is \code{TRUE}, the boxes are drawn
    with widths proportional to the square-roots of the number of
    observations in the groups.}
  \item{outline}{if \code{outline} is not true, the outliers are not
    drawn.}
  \item{notch.frac}{numeric in (0,1). When \code{notch=TRUE}, the
    fraction of the box width that the notches should use.}
  \item{border}{character or numeric (vector), the color of the box
    borders.  Is recycled for multiple boxes.  Is used as default for
    the \code{boxcol}, \code{medcol}, \code{whiskcol}, \code{staplecol},
    and \code{outcol} options (see below).
  }
  \item{col}{character or numeric; the color within the box; recycled for
    multiple boxes.  Is only used as default for \code{boxfill} and will
    be deprecated.}
  \item{log}{character, indicating if any axis should be drawn in
    logarithmic scale, as in \code{\link{plot.default}}.}
  \item{frame.plot}{logical, indicating if a \dQuote{frame}
    (\code{\link{box}}) should be drawn; defaults to \code{TRUE}, unless
    \code{axes = FALSE} is specified.}
  \item{horizontal}{logical indicating if the  boxplots should be
    horizontal; default \code{FALSE} means vertical boxes.}
  \item{add}{logical, if true \emph{add} boxplot to current plot.}
  \item{at}{numeric vector giving the locations where the boxplots
    should be drawn, particularly when \code{add = TRUE}; defaults to
    \code{1:n} where \code{n} is the number of boxes.}
  \item{show.names}{Set to \code{TRUE} or \code{FALSE} to override the
    defaults on whether an x-axis label is printed for each group.}
  \item{pars,\dots}{graphical parameters can be passed as arguments to
    this function, either as a list (\code{pars}) or
    normally(\code{\dots}), see the following.

    Currently, \code{yaxs} and \code{ylim} are used \sQuote{along the
      boxplot}, i.e., vertically, when \code{horizontal} is
    false. \code{xaxt}, \code{yaxt}, \code{las}, \code{cex.axis}, and
    \code{col.axis} are passed to \code{\link{axis}}, and \code{main},
    \code{cex.main}, \code{col.main}, \code{sub}, \code{cex.sub},
    \code{col.sub}, \code{xlab}, \code{ylab}, \code{cex.lab}, and
    \code{col.lab} are passed to \code{\link{title}}.

    The following arguments (or \code{pars} components) allow further
    customization of the boxplot graphics.  Their defaults are typically
    determined from the non-prefixed version (e.g., \code{boxlty} from
    \code{lty}), either from the specified argument or \code{pars}
    component or the corresponding \code{\link{par}} one.
    \describe{
      \item{boxwex:}{a scale factor to be applied to all boxes.  When there
	are only a few groups, the appearance of the plot can be improved by
	making the boxes narrower.}
      \item{staplewex, outwex:}{staple and outlier line width expansion,
	proportional to box width.}
      \item{boxlty, boxlwd, boxcol, boxfill:}{box outline type, width,
	color, and fill color.}
      \item{medlty, medlwd, medpch, medcex, medcol, medbg:}{median line type,
	line width, point character, point size expansion, color, and
	background color.  The default \code{medpch= NA} suppresses the
	point, and \code{medlty="blank"} does so for the line.
	Note that (since R 2.1.0) \code{medlwd} defaults to
      \eqn{3\times}{3 *} the \dQuote{default} \code{lwd}.}
      \item{whisklty, whisklwd, whiskcol:}{whisker line type, width, and
	color.}
      \item{staplelty, staplelwd, staplecol:}{staple (= end of whisker) line
	type, width, and color.}
      \item{outlty, outlwd, outpch, outcex, outcol, outbg:}{outlier line
	type, line width, point character, point size expansion, color, and
	background color.  The default \code{outlty= "blank"} suppresses the
	lines and \code{outpch=NA} suppresses points.}
    }
  }%.../pars
}
\value{
  An invisible vector, actually identical to the \code{at} argument,
  with the coordinates ("x" if horizontal is false, "y" otherwise) of
  box centers, useful for adding to the plot.
}
\author{The R Core development team and Arni Magnusson
  \email{arnima@u.washington.edu} who has provided most changes for the
  box*, med*, whisk*, staple*, and out* arguments.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth \& Brooks/Cole.
}
\examples{
set.seed(753)
(bx.p <- boxplot(split(rt(100, 4), gl(5,20))))
op <- par(mfrow= c(2,2))
bxp(bx.p, xaxt = "n")
bxp(bx.p, notch = TRUE, axes = FALSE, pch = 4, boxfill=1:5)
bxp(bx.p, notch = TRUE, boxfill= "lightblue", frame= FALSE, outl= FALSE,
    main = "bxp(*, frame= FALSE, outl= FALSE)")
bxp(bx.p, notch = TRUE, boxfill= "lightblue", border= 2:6, ylim = c(-4,4),
    pch = 22, bg = "green", log = "x", main = "... log='x', ylim=*")
par(op)
op <- par(mfrow= c(1,2))

## single group -- no label
boxplot (weight ~ group, data = PlantGrowth, subset = group=="ctrl")
## with label
bx <- boxplot(weight ~ group, data = PlantGrowth,
              subset = group=="ctrl", plot = FALSE)
bxp(bx,show.names=TRUE)
par(op)

%% examples for new (S+ like) features
z <- split(rnorm(1000), rpois(1000,2.2))
boxplot(z, whisklty=3, main="boxplot(z, whisklty = 3)")

## Colour support similar to plot.default:
op <- par(mfrow=1:2, bg="light gray", fg="midnight blue")
 boxplot(z,   col.axis="skyblue3", main="boxplot(*, col.axis=..,main=..)")
 plot(z[[1]], col.axis="skyblue3", main=   "plot(*, col.axis=..,main=..)")
 mtext("par(bg=\"light gray\", fg=\"midnight blue\")",
       outer = TRUE, line = -1.2)
par(op)

## Mimic S-Plus:
splus <- list(boxwex=0.4, staplewex=1, outwex=1, boxfill="grey40",
              medlwd=3, medcol="white", whisklty=3, outlty=1, outpch=NA)
boxplot(z, pars=splus)
## Recycled and "sweeping" parameters
op <- par(mfrow=c(1,2))
 boxplot(z, border=1:5, lty = 3, medlty = 1, medlwd = 2.5)
 boxplot(z, boxfill=1:3, pch=1:5, lwd = 1.5, medcol="white")
par(op)
## too many possibilities
boxplot(z, boxfill= "light gray", outpch = 21:25, outlty = 2,
        bg = "pink", lwd = 2, medcol = "dark blue", medcex = 2, medpch=20)
}
\keyword{aplot}
