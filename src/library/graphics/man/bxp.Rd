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
    notch.frac = 0.5, boxwex = 0.8, border = par("fg"), col = NULL,
    log = "", pars = NULL, frame.plot = axes, horizontal = FALSE,
    add = FALSE, at = NULL, show.names=NULL, \dots)
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
  \item{outline}{if \code{outline} is not true, the boxplot lines are
    not drawn.}
  \item{boxwex}{a scale factor to be applied to all boxes.  When there
    are only a few groups, the appearance of the plot can be improved by
    making the boxes narrower.}
  \item{notch.frac}{numeric in (0,1). When \code{notch=TRUE}, the
    fraction of the box width that the notches should use.}
  \item{border}{character, the color of the box borders.  Is recycled for
    multiple boxes.}
  \item{col}{character; the color within the box.  Is recycled for
    multiple boxes}
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
    normally(\code{\dots}).

    Currently, \code{pch}, \code{cex}, and \code{bg} are passed to
    \code{\link{points}},\cr
    \code{ylim} and \code{axes} to the main plot
    (\code{\link{plot.default}}), \code{xaxt}, \code{yaxt}, \code{las}
    to \code{\link{axis}} and the others to \code{\link{title}}.
  }
}
\value{
  An invisible vector, actually identical to the \code{at} argument,
  with the coordinates ("x" if horizontal is false, "y" otherwise) of
  box centers, useful for adding to the plot.
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
bxp(bx.p, notch = TRUE, axes = FALSE, pch = 4)
bxp(bx.p, notch = TRUE, col= "lightblue", frame= FALSE, outl= FALSE,
    main = "bxp(*, frame= FALSE, outl= FALSE)")
bxp(bx.p, notch = TRUE, col= "lightblue", border="red", ylim = c(-4,4),
    pch = 22, bg = "green", log = "x", main = "... log='x', ylim=*")
par(op)
op <- par(mfrow= c(1,2))
data(PlantGrowth)
## single group -- no label
boxplot(weight~group,data=PlantGrowth,subset=group=="ctrl")
bx<-boxplot(weight~group,data=PlantGrowth,subset=group=="ctrl",plot=FALSE)
## with label
bxp(bx,show.names=TRUE)
par(op)
}
\keyword{aplot}
