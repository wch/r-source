\name{boxplot}
\alias{boxplot}
\alias{boxplot.default}
\title{Box Plots}
\description{
  Produce box-and-whisker plot(s) of the given (grouped) values.
}
\usage{
\method{boxplot}{default}(x, \dots, range = 1.5, width = NULL, varwidth = FALSE,
        notch = FALSE, outline = TRUE, names, boxwex = 0.8, plot = TRUE,
        border = par("fg"), col = NULL, log = "", pars = NULL,
        horizontal = FALSE, add = FALSE, at = NULL)
}
\arguments{
  \item{x, \dots}{for specifying data from which the boxplots are to be
    produced as well as for giving graphical parameters.  The named
    arguments in this (more precisely, in \code{list(x, ...)}) are
    treated as graphical parameters in addition to the ones given by
    argument \code{pars}.  The other arguments specify the data, either
    as separate vectors, each corresponding to a component boxplot, or
    as a single list containing such vectors.  \code{\link{NA}}s are
    allowed in the data.}
  \item{range}{this determines how far the plot whiskers extend out
    from the box.  If \code{range} is positive, the whiskers extend
    to the most extreme data point which is no more than
    \code{range} times the interquartile range from the box. A value
    of zero causes the whiskers to extend to the data extremes.}
  \item{width}{a vector giving the relative widths of the boxes making
    up the plot.}
  \item{varwidth}{if \code{varwidth} is \code{TRUE}, the boxes are
    drawn with widths proportional to the square-roots of the number
    of observations in the groups.}
  \item{notch}{if \code{notch} is \code{TRUE}, a notch is drawn in
    each side of the boxes.  If the notches of two plots do not
    overlap then the medians are significantly different at the 5
    percent level.}
  \item{outline}{if \code{outline} is not true, the boxplot lines are
    not drawn.}% the argument name is most ugly but S+ compatible
  \item{names}{group labels which will be printed under each boxplot.}
  \item{boxwex}{a scale factor to be applied to all boxes.  When there
    are only a few groups, the appearance of the plot can be improved
    by making the boxes narrower.}
  \item{plot}{if \code{TRUE} (the default) then a boxplot is
    produced.  If not, the summaries which the boxplots are based on
    are returned.}
  \item{border}{an optional vector of colors for the outlines of the
    boxplots.  The values in \code{border} are recycled if the
    length of \code{border} is less than the number of plots.}
  \item{col}{if \code{col} is non-null it is assumed to contain colors
    to be used to col the bodies of the box plots.}
  \item{log}{character indicating if x or y or both coordinates should
    be plotted in log scale.}
  \item{pars}{graphical parameters can also be passed as arguments to
    \code{boxplot}.}
  \item{horizontal}{logical indicating if the boxplots should be
    horizontal; default \code{FALSE} means vertical boxes.}
  \item{add}{logical, if true \emph{add} boxplot to current plot.}
  \item{at}{numeric vector giving the locations where the boxplots should
    be drawn, particularly when \code{add = TRUE};
    defaults to \code{1:n} where \code{n} is the number of boxes.}
}
\details{
  The generic function \code{boxplot} currently has a default method
  (\code{boxplot.default}) and a formula interface
  (\code{\link{boxplot.formula}}).
}
\value{
  List with the following components:
  \item{stats}{a matrix, each column contains the extreme of the lower
    whisker, the lower hinge, the median, the upper hinge and the
    extreme of the upper whisker for one group/plot.}
  \item{n}{a vector with the number of observations in each group.}
  \item{conf}{a matrix where each column contains the lower and upper
    extremes of the notch.}
  \item{out}{the values of any data points which lie beyond the
    extremes of the whiskers.}
  \item{group}{a vector of the same length as \code{out} whose elements
    indicate which group the outlier belongs to}
  \item{names}{a vector of names for the groups}
}
\seealso{
  \code{\link{boxplot.formula}} for the formula interface;
  \code{\link{boxplot.stats}} which does the computation,
  \code{\link{bxp}} for the plotting;
  and \code{\link{stripchart}} for an alternative (with small data
  sets).
}
\examples{
## boxplot on a formula:
data(InsectSprays)
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")
# *add* notches (somewhat funny here):
boxplot(count ~ spray, data = InsectSprays,
        notch = TRUE, add = TRUE, col = "blue")

data(OrchardSprays)
boxplot(decrease ~ treatment, data = OrchardSprays,
        log = "y", col="bisque")

rb <- boxplot(decrease ~ treatment, data = OrchardSprays, col="bisque")
title("Comparing boxplot()s and non-robust mean +/- SD")

mn.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, mean)
sd.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, sd)
xi <- 0.3 + seq(rb$n)
points(xi, mn.t, col = "orange", pch = 18)
arrows(xi, mn.t - sd.t, xi, mn.t + sd.t,
       code = 3, col = "pink", angle = 75, length = .1)

## boxplot on a matrix:
mat <- cbind(Uni05 = (1:100)/21, Norm = rnorm(100),
             T5 = rt(100, df = 5), Gam2 = rgamma(100, shape = 2))
boxplot(data.frame(mat), main = "boxplot(data.frame(mat), main = ...)")
par(las=1)# all axis labels horizontal
boxplot(data.frame(mat), main = "boxplot(*, horizontal = TRUE)",
        horizontal = TRUE)

## Using `at = ' and adding boxplots -- example idea by Roger Bivand :

data(ToothGrowth)
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset= supp == "VC", col="yellow",
        main="Guinea Pigs' Tooth Growth",
        xlab="Vitamin C dose mg",
        ylab="tooth length", ylim=c(0,35))
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset= supp == "OJ", col="orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))
}
\keyword{hplot}
