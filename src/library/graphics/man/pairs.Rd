% File src/library/graphics/man/pairs.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{pairs}
\alias{pairs}
\alias{pairs.default}
\alias{pairs.formula}

\title{Scatterplot Matrices}
\description{
  A matrix of scatterplots is produced.
}
\usage{
pairs(x, \dots)

\method{pairs}{formula}(formula, data = NULL, \dots, subset,
      na.action = stats::na.pass)

\method{pairs}{default}(x, labels, panel = points, \dots,
      horInd = 1:nc, verInd = 1:nc,
      lower.panel = panel, upper.panel = panel,
      diag.panel = NULL, text.panel = textPanel,
      label.pos = 0.5 + has.diag/3, line.main = 3,
      cex.labels = NULL, font.labels = 1,
      row1attop = TRUE, gap = 1, log = "")
}
\arguments{
  \item{x}{the coordinates of points given as numeric columns of a
    matrix or data frame.  Logical and factor columns are converted to
    numeric in the same way that \code{\link{data.matrix}} does.
  }
  \item{formula}{a formula, such as \code{~ x + y + z}.  Each term will
    give a separate variable in the pairs plot, so terms should be
    numeric vectors.  (A response will be interpreted as another
    variable, but not treated specially, so it is confusing to use one.)}
  \item{data}{a data.frame (or list) from which the variables in
    \code{formula} should be taken.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used for plotting.}
  \item{na.action}{a function which indicates what should happen
    when the data contain \code{NA}s.  The default is to pass missing
    values on to the panel functions, but \code{na.action = na.omit}
    will cause cases with missing values in any of the variables to be
    omitted entirely.}
  \item{labels}{the names of the variables.}
  \item{panel}{\code{function(x, y, \dots)} which is used to plot
    the contents of each panel of the display.}
  \item{\dots}{
    arguments to be passed to or from methods.

    Also, \link{graphical parameters} can be given as can arguments to
    \code{plot} such as \code{main}.  \code{par("oma")} will be set
    appropriately unless specified.
  }
  \item{horInd, verInd}{The (numerical) indices of the variables to be
    plotted on the horizontal and vertical axes respectively.}
  \item{lower.panel, upper.panel}{separate panel functions (or
    \code{NULL}) to be used below and above the diagonal respectively.}
  \item{diag.panel}{optional \code{function(x, \dots)} to be
    applied on the diagonals.}
  \item{text.panel}{optional \code{function(x, y, labels, cex,
      font, \dots)} to be applied on the diagonals.}
  \item{label.pos}{\code{y} position of labels in the text panel.}
  \item{line.main}{if \code{main} is specified, \code{line.main} gives
    the \code{line} argument to \code{\link{mtext}()} which draws the
    title.  You may want to specify \code{oma} when changing \code{line.main}.}
  \item{cex.labels, font.labels}{graphics parameters for the text panel.}
  \item{row1attop}{logical. Should the layout be matrix-like with row 1
    at the top, or graph-like with row 1 at the bottom?}
  \item{gap}{distance between subplots, in margin lines.}
  \item{log}{a character string indicating if logarithmic axes are to be
    used: see \code{\link{plot.default}}.  \code{log = "xy"} specifies
    logarithmic axes for all variables.}
}
\details{
  The \eqn{ij}th scatterplot contains \code{x[,i]} plotted against
  \code{x[,j]}.  The scatterplot can be customised by setting panel
  functions to appear as something completely different. The
  off-diagonal panel functions are passed the appropriate columns of
  \code{x} as \code{x} and \code{y}: the diagonal panel function (if
  any) is passed a single column, and the \code{text.panel} function is
  passed a single \code{(x, y)} location and the column name.
  Setting some of these panel functions to \code{\link{NULL}} is
  equivalent to \emph{not} drawing anything there.

  The \link{graphical parameters} \code{pch} and \code{col} can be used
  to specify a vector of plotting symbols and colors to be used in the
  plots.

  The \link{graphical parameter} \code{oma} will be set by
  \code{pairs.default} unless supplied as an argument.

  A panel function should not attempt to start a new plot, but just plot
  within a given coordinate system: thus \code{plot} and \code{boxplot}
  are not panel functions.

  By default, missing values are passed to the panel functions and will
  often be ignored within a panel.  However, for the formula method and
  \code{na.action = na.omit}, all cases which contain a missing values for
  any of the variables are omitted completely (including when the scales
  are selected).
  
  Arguments \code{horInd} and \code{verInd} were introduced in \R
  3.2.0. If given the same value they can be used to select or re-order
  variables: with different ranges of consecutive values they can be
  used to plot rectangular windows of a full pairs plot; in the latter
  case \sQuote{diagonal} refers to the diagonal of the full plot. 
}
\author{
  Enhancements for \R 1.0.0 contributed by Dr. Jens
  Oehlschlaegel-Akiyoshi and R-core members.
}
\references{
  Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988)
  \emph{The New S Language}.
  Wadsworth & Brooks/Cole.
}
\examples{
pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species",
      pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

## formula method
pairs(~ Fertility + Education + Catholic, data = swiss,
      subset = Education < 20, main = "Swiss data, Education < 20")

pairs(USJudgeRatings)
## show only lower triangle (and suppress labeling for whatever reason):
pairs(USJudgeRatings, text.panel = NULL, upper.panel = NULL)

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(USJudgeRatings[1:5], panel = panel.smooth,
      cex = 1.5, pch = 24, bg = "light blue",
      diag.panel = panel.hist, cex.labels = 2, font.labels = 2)

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(USJudgeRatings, lower.panel = panel.smooth, upper.panel = panel.cor)

pairs(iris[-5], log = "xy") # plot all variables on log scale
pairs(iris, log = 1:4, # log the first four
      main = "Lengths and Widths in [log]", line.main=1.5, oma=c(2,2,3,2))
}
\keyword{hplot}
