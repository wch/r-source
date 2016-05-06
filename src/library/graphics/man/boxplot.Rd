% File src/library/graphics/man/boxplot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2016 R Core Team
% Distributed under GPL 2 or later

\name{boxplot}
\alias{boxplot}
\alias{boxplot.default}
\alias{boxplot.formula}
\title{Box Plots}
\description{
  Produce box-and-whisker plot(s) of the given (grouped) values.
}
\usage{
boxplot(x, \dots)

\method{boxplot}{formula}(formula, data = NULL, \dots, subset, na.action = NULL,
        drop = FALSE, sep = ".", lex.order = FALSE)

\method{boxplot}{default}(x, \dots, range = 1.5, width = NULL, varwidth = FALSE,
        notch = FALSE, outline = TRUE, names, plot = TRUE,
        border = par("fg"), col = NULL, log = "",
        pars = list(boxwex = 0.8, staplewex = 0.5, outwex = 0.5),
        horizontal = FALSE, add = FALSE, at = NULL)
}
\arguments{
  \item{formula}{a formula, such as \code{y ~ grp}, where \code{y} is a
    numeric vector of data values to be split into groups according to
    the grouping variable \code{grp} (usually a factor).}
  \item{data}{a data.frame (or list) from which the variables in
    \code{formula} should be taken.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used for plotting.}
  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is to ignore missing
    values in either the response or the group.}
  \item{drop, sep, lex.order}{passed to \code{\link{split.default}}, see there.}
  \item{x}{for specifying data from which the boxplots are to be
    produced. Either a numeric vector, or a single list containing such
    vectors. Additional unnamed arguments specify further data
    as separate vectors (each corresponding to a component boxplot).
    \code{\link{NA}}s are allowed in the data.}
  \item{\dots}{For the \code{formula} method, named arguments to be passed to
    the default method.

    For the default method, unnamed arguments are additional data
    vectors (unless \code{x} is a list when they are ignored), and named
    arguments are arguments and \link{graphical parameters} to be passed
    to \code{\link{bxp}} in addition to the ones given by argument
    \code{pars} (and override those in \code{pars}). Note that
    \code{bxp} may or may not make use of graphical parameters it is
    passed: see its documentation.
  }
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
    overlap this is \sQuote{strong evidence} that the two medians differ
    (Chambers \emph{et al}, 1983, p.\sspace{}62).  See \code{\link{boxplot.stats}}
    for the calculations used.}
  \item{outline}{if \code{outline} is not true, the outliers are
    not drawn (as points whereas S+ uses lines).}% the argument name is most ugly but S+ compatible
  \item{names}{group labels which will be printed under each boxplot.
    Can be a character vector or an \link{expression} (see
    \link{plotmath}).}
  \item{boxwex}{a scale factor to be applied to all boxes.  When there
    are only a few groups, the appearance of the plot can be improved
    by making the boxes narrower.}
  \item{staplewex}{staple line width expansion, proportional to box
    width.}
  \item{outwex}{outlier line width expansion, proportional to box
    width.}
  \item{plot}{if \code{TRUE} (the default) then a boxplot is
    produced.  If not, the summaries which the boxplots are based on
    are returned.}
  \item{border}{an optional vector of colors for the outlines of the
    boxplots.  The values in \code{border} are recycled if the
    length of \code{border} is less than the number of plots.}
  \item{col}{if \code{col} is non-null it is assumed to contain colors
    to be used to colour the bodies of the box plots. By default they
    are in the background colour.}
  \item{log}{character indicating if x or y or both coordinates should
    be plotted in log scale.}
  \item{pars}{a list of (potentially many) more graphical parameters,
    e.g., \code{boxwex} or \code{outpch}; these are passed to
    \code{\link{bxp}} (if \code{plot} is true); for details, see there.}
  \item{horizontal}{logical indicating if the boxplots should be
    horizontal; default \code{FALSE} means vertical boxes.}
  \item{add}{logical, if true \emph{add} boxplot to current plot.}
  \item{at}{numeric vector giving the locations where the boxplots should
    be drawn, particularly when \code{add = TRUE};
    defaults to \code{1:n} where \code{n} is the number of boxes.}
}
\details{
  The generic function \code{boxplot} currently has a default method
  (\code{boxplot.default}) and a formula interface (\code{boxplot.formula}).

  If multiple groups are supplied either as multiple arguments or via a
  formula, parallel boxplots will be plotted, in the order of the
  arguments or the order of the levels of the factor (see
  \code{\link{factor}}).

  Missing values are ignored when forming boxplots.
}
\value{
  List with the following components:
  \item{stats}{a matrix, each column contains the extreme of the lower
    whisker, the lower hinge, the median, the upper hinge and the
    extreme of the upper whisker for one group/plot.  If all the inputs
    have the same class attribute, so will this component.}
  \item{n}{a vector with the number of observations in each group.}
  \item{conf}{a matrix where each column contains the lower and upper
    extremes of the notch.}
  \item{out}{the values of any data points which lie beyond the
    extremes of the whiskers.}
  \item{group}{a vector of the same length as \code{out} whose elements
    indicate to which group the outlier belongs.}
  \item{names}{a vector of names for the groups.}
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.

  Chambers, J. M., Cleveland, W. S., Kleiner, B. and Tukey, P. A. (1983)
  \emph{Graphical Methods for Data Analysis.}  Wadsworth & Brooks/Cole.

  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.

  See also \code{\link{boxplot.stats}}.
}
\seealso{
  \code{\link{boxplot.stats}} which does the computation,
  \code{\link{bxp}} for the plotting and more examples;
  and \code{\link{stripchart}} for an alternative (with small data
  sets).
}
\examples{
## boxplot on a formula:
boxplot(count ~ spray, data = InsectSprays, col = "lightgray")
# *add* notches (somewhat funny here):
boxplot(count ~ spray, data = InsectSprays,
        notch = TRUE, add = TRUE, col = "blue")

boxplot(decrease ~ treatment, data = OrchardSprays,
        log = "y", col = "bisque")

rb <- boxplot(decrease ~ treatment, data = OrchardSprays, col = "bisque")
title("Comparing boxplot()s and non-robust mean +/- SD")

mn.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, mean)
sd.t <- tapply(OrchardSprays$decrease, OrchardSprays$treatment, sd)
xi <- 0.3 + seq(rb$n)
points(xi, mn.t, col = "orange", pch = 18)
arrows(xi, mn.t - sd.t, xi, mn.t + sd.t,
       code = 3, col = "pink", angle = 75, length = .1)

## boxplot on a matrix:
mat <- cbind(Uni05 = (1:100)/21, Norm = rnorm(100),
             `5T` = rt(100, df = 5), Gam2 = rgamma(100, shape = 2))
boxplot(as.data.frame(mat),
        main = "boxplot(as.data.frame(mat), main = ...)")
par(las = 1) # all axis labels horizontal
boxplot(as.data.frame(mat), main = "boxplot(*, horizontal = TRUE)",
        horizontal = TRUE)

## Using 'at = ' and adding boxplots -- example idea by Roger Bivand :

boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset = supp == "OJ", col = "orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))

## With less effort (slightly different) using factor *interaction*:
boxplot(len ~ dose:supp, data = ToothGrowth,
        boxwex = 0.5, col = c("orange", "yellow"),
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg", ylab = "tooth length",
        sep = ":", lex.order = TRUE, ylim = c(0, 35), yaxs = "i")

## more examples in  help(bxp)
}
\keyword{hplot}
