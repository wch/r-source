% File src/library/graphics/man/mosaicplot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2019 R Core Team
% Distributed under GPL 2 or later

\name{mosaicplot}
\alias{mosaicplot}
\alias{mosaicplot.default}
\alias{mosaicplot.formula}
\encoding{UTF-8}
\title{Mosaic Plots}
\description{Plots a mosaic on the current graphics device.}
\usage{
mosaicplot(x, \dots)

\method{mosaicplot}{default}(x, main = deparse1(substitute(x)),
           sub = NULL, xlab = NULL, ylab = NULL,
           sort = NULL, off = NULL, dir = NULL,
           color = NULL, shade = FALSE, margin = NULL,
           cex.axis = 0.66, las = par("las"), border = NULL,
           type = c("pearson", "deviance", "FT"), \dots)

\method{mosaicplot}{formula}(formula, data = NULL, \dots,
           main = deparse1(substitute(data)), subset,
           na.action = stats::na.omit)
}
\arguments{
  \item{x}{a contingency table in array form, with optional category
    labels specified in the \code{dimnames(x)} attribute.  The table is
    best created by the \code{table()} command.}
  \item{main}{character string for the mosaic title.}
  \item{sub}{character string for the mosaic sub-title (at bottom).}
  \item{xlab, ylab}{x- and y-axis labels used for the plot; by default,
    the first and second element of \code{names(dimnames(X))} (i.e., the
    name of the first and second variable in \code{X}).}
  \item{sort}{vector ordering of the variables, containing a permutation
    of the integers \code{1:length(dim(x))} (the default).}
  \item{off}{vector of offsets to determine percentage spacing at each
    level of the mosaic (appropriate values are between 0 and 20,
    and the default is 20 times the number of splits for 2-dimensional
    tables, and 10 otherwise).  Rescaled to maximally 50, and recycled if
    necessary.}
  \item{dir}{vector of split directions (\code{"v"} for vertical and
    \code{"h"} for horizontal) for each level of the mosaic, one
    direction for each dimension of the contingency table.  The
    default consists of alternating directions, beginning with a
    vertical split.}
  \item{color}{logical or (recycling) vector of colors for color
    shading, used only when \code{shade} is \code{FALSE}, or \code{NULL}
    (default).  By default, grey boxes are drawn.  \code{color = TRUE}
    uses a gamma-corrected grey palette.  \code{color = FALSE} gives empty
    boxes with no shading.}
  \item{shade}{a logical indicating whether to produce extended mosaic
    plots, or a numeric vector of at most 5 distinct positive numbers
    giving the absolute values of the cut points for the residuals.  By
    default, \code{shade} is \code{FALSE}, and simple mosaics are
    created.  Using \code{shade = TRUE} cuts absolute values at 2 and
    4.}
  \item{margin}{a list of vectors with the marginal totals to be fit in
    the log-linear model.  By default, an independence model is fitted.
    See \code{\link{loglin}} for further information.}
  \item{cex.axis}{The magnification to be used for axis annotation,
    as a multiple of \code{par("cex")}.}
  \item{las}{numeric; the style of axis labels, see \code{\link{par}}.}
  \item{border}{colour of borders of cells: see \code{\link{polygon}}.}
  \item{type}{a character string indicating the type of residual to be
    represented.  Must be one of \code{"pearson"} (giving components of
    Pearson's \eqn{\chi^2}{chi-squared}), \code{"deviance"} (giving
    components of the likelihood ratio \eqn{\chi^2}{chi-squared}), or
    \code{"FT"} for the Freeman-Tukey residuals.  The value of this
    argument can be abbreviated.}
  \item{formula}{a formula, such as \code{y ~ x}.}
  \item{data}{a data frame (or list), or a contingency table from which
    the variables in \code{formula} should be taken.}
  \item{\dots}{further arguments to be passed to or from methods.}
  \item{subset}{an optional vector specifying a subset of observations
    in the data frame to be used for plotting.}
  \item{na.action}{a function which indicates what should happen
    when the data contains variables to be cross-tabulated, and these
    variables contain \code{NA}s.  The default is to omit cases which
    have an \code{NA} in any variable.  Since the tabulation will omit
    all cases containing missing values, this will only be useful if the
    \code{na.action} function replaces missing values.}
}
\details{
  This is a generic function.  It currently has a default method
  (\code{mosaicplot.default}) and a formula interface
  (\code{mosaicplot.formula}).

  Extended mosaic displays visualize standardized residuals of a
  loglinear model for the table by color and outline of the mosaic's
  tiles.  (Standardized residuals are often referred to a standard
  normal distribution.)  Cells representing negative residuals are drawn
  in shaded of red and with broken borders; positive ones are drawn in
  blue with solid borders.

  For the formula method, if \code{data} is an object inheriting from
  class \code{"table"} or class \code{"ftable"} or an array with more
  than 2 dimensions, it is taken as a contingency table, and hence all
  entries should be non-negative.  In this case the left-hand side of
  \code{formula} should be empty and the variables on the right-hand
  side should be taken from the names of the dimnames attribute of the
  contingency table.  A marginal table of these variables is computed,
  and a mosaic plot of that table is produced.

  Otherwise, \code{data} should be a data frame or matrix, list or
  environment containing the variables to be cross-tabulated.  In this
  case, after possibly selecting a subset of the data as specified by
  the \code{subset} argument, a contingency table is computed from the
  variables given in \code{formula}, and a mosaic is produced from
  this.

  See Emerson (1998) for more information and a case study with
  television viewer data from Nielsen Media Research.

  Missing values are not supported except via an \code{na.action}
  function when \code{data} contains variables to be cross-tabulated.

  A more flexible and extensible implementation of mosaic plots written
  in the grid graphics system is provided in the function
  \code{\link[vcd]{mosaic}} in the contributed package \CRANpkg{vcd}
  (Meyer, Zeileis and Hornik, 2006).
}
\author{
  S-PLUS original by John Emerson \email{john.emerson@yale.edu}.
  Originally modified and enhanced for \R by Kurt Hornik.
}
\references{
  Hartigan, J.A., and Kleiner, B. (1984).
  A mosaic of television ratings.
  \emph{The American Statistician}, \bold{38}, 32--35.
  \doi{10.2307/2683556}.

  Emerson, J. W. (1998).
  Mosaic displays in S-PLUS: A general implementation and a case study.
  \emph{Statistical Computing and Graphics Newsletter (ASA)},
  \bold{9}, 1, 17--23.

  Friendly, M. (1994).
  Mosaic displays for multi-way contingency tables.
  \emph{Journal of the American Statistical Association}, \bold{89},
  190--200.
  \doi{10.2307/2291215}.

  Meyer, D., Zeileis, A., and Hornik, K. (2006)  
  The strucplot Framework: Visualizing Multi-Way Contingency Tables with
  \pkg{vcd}.
  \emph{Journal of Statistical Software}, \bold{17(3)}, 1--48.
  \doi{10.18637/jss.v017.i03}.
}
\seealso{
  \code{\link{assocplot}},
  \code{\link{loglin}}.
}
\examples{
require(stats)
mosaicplot(Titanic, main = "Survival on the Titanic", color = TRUE)
## Formula interface for tabulated data:
mosaicplot(~ Sex + Age + Survived, data = Titanic, color = TRUE)

mosaicplot(HairEyeColor, shade = TRUE)
## Independence model of hair and eye color and sex.  Indicates that
## there are more blue eyed blonde females than expected in the case
## of independence and too few brown eyed blonde females.
## The corresponding model is:
fm <- loglin(HairEyeColor, list(1, 2, 3))
pchisq(fm$pearson, fm$df, lower.tail = FALSE)

mosaicplot(HairEyeColor, shade = TRUE, margin = list(1:2, 3))
## Model of joint independence of sex from hair and eye color.  Males
## are underrepresented among people with brown hair and eyes, and are
## overrepresented among people with brown hair and blue eyes.
## The corresponding model is:
fm <- loglin(HairEyeColor, list(1:2, 3))
pchisq(fm$pearson, fm$df, lower.tail = FALSE)

## Formula interface for raw data: visualize cross-tabulation of numbers
## of gears and carburettors in Motor Trend car data.
mosaicplot(~ gear + carb, data = mtcars, color = TRUE, las = 1)
# color recycling
mosaicplot(~ gear + carb, data = mtcars, color = 2:3, las = 1)
}
\keyword{hplot}
