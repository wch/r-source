% File src/library/graphics/man/spineplot.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2018 R Core Team
% Distributed under GPL 2 or later

\name{spineplot}
\alias{spineplot}
\alias{spineplot.default}
\alias{spineplot.formula}
\title{Spine Plots and Spinograms}
\description{
  Spine plots are a special cases of mosaic plots, and can be seen as
  a generalization of stacked (or highlighted) bar plots. Analogously,
  spinograms are an extension of histograms.
}
\usage{
spineplot(x, \dots)

\method{spineplot}{default}(x, y = NULL,
          breaks = NULL, tol.ylab = 0.05, off = NULL,
          ylevels = NULL, col = NULL,
          main = "", xlab = NULL, ylab = NULL,
          xaxlabels = NULL, yaxlabels = NULL,
          xlim = NULL, ylim = c(0, 1), axes = TRUE, \dots)

\method{spineplot}{formula}(formula, data = NULL,
          breaks = NULL, tol.ylab = 0.05, off = NULL,
          ylevels = NULL, col = NULL,
          main = "", xlab = NULL, ylab = NULL,
          xaxlabels = NULL, yaxlabels = NULL,
          xlim = NULL, ylim = c(0, 1), axes = TRUE, \dots,
          subset = NULL, drop.unused.levels = FALSE)
}
\arguments{
  \item{x}{an object, the default method expects either a single variable
    (interpreted to be the explanatory variable) or a 2-way table.  See
    details.}
  \item{y}{a \code{"factor"} interpreted to be the dependent variable}
  \item{formula}{a \code{"formula"} of type \code{y ~ x} with a single
    dependent \code{"factor"} and a single explanatory variable.}
  \item{data}{an optional data frame.}
  \item{breaks}{if the explanatory variable is numeric, this controls how
    it is discretized. \code{breaks} is passed to \code{\link{hist}} and can
    be a list of arguments.}
  \item{tol.ylab}{convenience tolerance parameter for y-axis annotation.
    If the distance between two labels drops under this threshold, they are
    plotted equidistantly.}
  \item{off}{vertical offset between the bars (in per cent). It is fixed to
    \code{0} for spinograms and defaults to \code{2} for spine plots.}
  \item{ylevels}{a character or numeric vector specifying in which order
    the levels of the dependent variable should be plotted.}
  \item{col}{a vector of fill colors of the same length as \code{levels(y)}.
    The default is to call \code{\link{gray.colors}}.}
  \item{main, xlab, ylab}{character strings for annotation}
  \item{xaxlabels, yaxlabels}{character vectors for annotation of x and y axis.
    Default to \code{levels(y)} and \code{levels(x)}, respectively for the
    spine plot. For \code{xaxlabels} in the spinogram, the breaks are
    used.}
  \item{xlim, ylim}{the range of x and y values with sensible defaults.}
  \item{axes}{logical.  If \code{FALSE} all axes (including those giving
    level names) are suppressed.}
  \item{\dots}{additional arguments passed to \code{\link{rect}}.}
  \item{subset}{an optional vector specifying a subset of observations
    to be used for plotting.}
  \item{drop.unused.levels}{should factors have unused levels dropped?
    Defaults to \code{FALSE}.}
}
\details{
  \code{spineplot} creates either a spinogram or a spine plot.  It can
  be called via \code{spineplot(x, y)} or \code{spineplot(y ~ x)} where
  \code{y} is interpreted to be the dependent variable (and has to be
  categorical) and \code{x} the explanatory variable.  \code{x} can be
  either categorical (then a spine plot is created) or numerical (then a
  spinogram is plotted).  Additionally, \code{spineplot} can also be
  called with only a single argument which then has to be a 2-way table,
  interpreted to correspond to \code{table(x, y)}.

  Both, spine plots and spinograms, are essentially mosaic plots with
  special formatting of spacing and shading.  Conceptually, they plot
  \eqn{P(y | x)} against \eqn{P(x)}.  For the spine plot (where both
  \eqn{x} and \eqn{y} are categorical), both quantities are approximated
  by the corresponding empirical relative frequencies.  For the
  spinogram (where \eqn{x} is numerical), \eqn{x} is first discretized
  (by calling \code{\link{hist}} with \code{breaks} argument) and then
  empirical relative frequencies are taken.

  Thus, spine plots can also be seen as a generalization of stacked bar
  plots where not the heights but the widths of the bars corresponds to
  the relative frequencies of \code{x}. The heights of the bars then
  correspond to the conditional relative frequencies of \code{y} in
  every \code{x} group. Analogously, spinograms extend stacked
  histograms.
}
\value{
  The table visualized is returned invisibly.
}
\seealso{
  \code{\link{mosaicplot}}, \code{\link{hist}}, \code{\link{cdplot}}
}
\references{
  Friendly, M. (1994).
  Mosaic displays for multi-way contingency tables.
  \emph{Journal of the American Statistical Association}, \bold{89},
  190--200.
  \doi{10.2307/2291215}.

  Hartigan, J.A., and Kleiner, B. (1984).
  A mosaic of television ratings.
  \emph{The American Statistician}, \bold{38}, 32--35.
  \doi{10.2307/2683556}.

  Hofmann, H., Theus, M. (2005),
  \emph{Interactive graphics for visualizing conditional distributions}.
  Unpublished Manuscript.

  Hummel, J. (1996).
  Linked bar charts: Analysing categorical data graphically.
  \emph{Computational Statistics}, \bold{11}, 23--33.
}
\author{
  Achim Zeileis \email{Achim.Zeileis@R-project.org}
}
\examples{
## treatment and improvement of patients with rheumatoid arthritis
treatment <- factor(rep(c(1, 2), c(43, 41)), levels = c(1, 2),
                    labels = c("placebo", "treated"))
improved <- factor(rep(c(1, 2, 3, 1, 2, 3), c(29, 7, 7, 13, 7, 21)),
                   levels = c(1, 2, 3),
                   labels = c("none", "some", "marked"))

## (dependence on a categorical variable)
(spineplot(improved ~ treatment))

## applications and admissions by department at UC Berkeley
## (two-way tables)
(spineplot(margin.table(UCBAdmissions, c(3, 2)),
           main = "Applications at UCB"))
(spineplot(margin.table(UCBAdmissions, c(3, 1)),
           main = "Admissions at UCB"))

## NASA space shuttle o-ring failures
fail <- factor(c(2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 2, 1, 2, 1,
                 1, 1, 1, 2, 1, 1, 1, 1, 1),
               levels = c(1, 2), labels = c("no", "yes"))
temperature <- c(53, 57, 58, 63, 66, 67, 67, 67, 68, 69, 70, 70,
                 70, 70, 72, 73, 75, 75, 76, 76, 78, 79, 81)

## (dependence on a numerical variable)
(spineplot(fail ~ temperature))
(spineplot(fail ~ temperature, breaks = 3))
(spineplot(fail ~ temperature, breaks = quantile(temperature)))

## highlighting for failures
spineplot(fail ~ temperature, ylevels = 2:1)
}
\keyword{hplot}
