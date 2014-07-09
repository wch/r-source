% File src/library/stats/man/termplot.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2014 R Core Team
% Distributed under GPL 2 or later

\name{termplot}
\alias{termplot}
\title{Plot Regression Terms}
\description{
  Plots regression terms against their predictors, optionally with
  standard errors and partial residuals added.
}
\usage{
termplot(model, data = NULL, envir = environment(formula(model)),
         partial.resid = FALSE, rug = FALSE,
         terms = NULL, se = FALSE,
         xlabs = NULL, ylabs = NULL, main = NULL,
         col.term = 2, lwd.term = 1.5,
         col.se = "orange", lty.se = 2, lwd.se = 1,
         col.res = "gray", cex = 1, pch = par("pch"),
         col.smth = "darkred", lty.smth = 2, span.smth = 2/3,
         ask = dev.interactive() && nb.fig < n.tms,
         use.factor.levels = TRUE, smooth = NULL, ylim = "common",
         plot = TRUE, transform.x = FALSE, \dots)
}
\arguments{
  \item{model}{fitted model object}
  \item{data}{data frame in which variables in \code{model} can be
    found}
  \item{envir}{environment in which variables in \code{model} can be found}
  \item{partial.resid}{logical; should partial residuals be plotted?}
  \item{rug}{add \link{rug}plots (jittered 1-d histograms) to the axes?}
  \item{terms}{which terms to plot (default \code{NULL} means all
    terms); a vector passed to
    \code{\link{predict}(.., type = "terms", terms = *)}.}
  \item{se}{plot pointwise standard errors?}
  \item{xlabs}{vector of labels for the x axes}
  \item{ylabs}{vector of labels for the y axes}
  \item{main}{logical, or vector of main titles;  if \code{TRUE}, the
    model's call is taken as main title, \code{NULL} or \code{FALSE} mean
    no titles.}
  \item{col.term, lwd.term}{color and line width for the \sQuote{term curve},
    see \code{\link{lines}}.}
  \item{col.se, lty.se, lwd.se}{color, line type and line width for the
    \sQuote{twice-standard-error curve} when \code{se = TRUE}.}
  \item{col.res, cex, pch}{color, plotting character expansion and type
    for partial residuals, when \code{partial.resid = TRUE}, see
    \code{\link{points}}.}
   \item{ask}{logical; if \code{TRUE}, the user is \emph{ask}ed before
     each plot, see \code{\link{par}(ask=.)}.}
   \item{use.factor.levels}{Should x-axis ticks use factor levels or
     numbers for factor terms?}
   \item{smooth}{\code{NULL} or a function with the same arguments as
     \code{\link{panel.smooth}} to draw a smooth through the partial
     residuals for non-factor terms}
   \item{lty.smth, col.smth, span.smth}{Passed to \code{smooth}}
   \item{ylim}{an optional range for the y axis, or \code{"common"} when
     a range sufficient for all the plot will be computed, or
     \code{"free"} when limits are computed for each plot.}
   \item{plot}{if set to \code{FALSE} plots are not produced: instead a
     list is returned containing the data that would have been plotted.}
   \item{transform.x}{logical vector; if an element (recycled as necessary)
     is \code{TRUE}, partial residuals for the corresponding term are
     plotted against transformed values.
     The model response is then a straight line, allowing a
     ready comparison against the data or against the curve
     obtained from \code{smooth-panel.smooth}.}
   \item{\dots}{other graphical parameters.}
}
\details{
  The \code{model} object must have a \code{predict} method that accepts
  \code{type = "terms"}, e.g., \code{\link{glm}} in the \pkg{stats} package,
  \code{\link[survival]{coxph}} and \code{\link[survival]{survreg}} in
  the \CRANpkg{survival} package.

  For the \code{partial.resid = TRUE} option \code{model} must have a
  \code{\link{residuals}} method that accepts \code{type = "partial"},
  which \code{\link{lm}} and \code{\link{glm}} do.

  The \code{data} argument should rarely be needed, but in some cases
  \code{termplot} may be unable to reconstruct the original data
  frame. Using \code{na.action=na.exclude} makes these problems less likely.

  Nothing sensible happens for interaction terms, and they may cause errors.

  The \code{plot = FALSE} option is useful when some special action is needed,
  e.g. to overlay the results of two different models or to plot
  confidence bands.
}
\value{
  For \code{plot = FALSE}, a list with one element for each plot which
  would have been produced.  Each element of the list is a data frame
  with variables \code{x}, \code{y}, and optionally the pointwise
  standard errors \code{se}. For continuous predictors \code{x} will
  contain the ordered unique values and for a factor it will be a factor
  containing one instance of each level.  The list has attribute
  \code{"constant"} copied from the predicted terms object.

  Otherwise, the number of terms, invisibly.
}
\seealso{For (generalized) linear models, \code{\link{plot.lm}} and
  \code{\link{predict.glm}}.}
\examples{
require(graphics)

had.splines <- "package:splines" \%in\% search()
if(!had.splines) rs <- require(splines)
x <- 1:100
z <- factor(rep(LETTERS[1:4], 25))
y <- rnorm(100, sin(x/10)+as.numeric(z))
model <- glm(y ~ ns(x, 6) + z)

par(mfrow = c(2,2)) ## 2 x 2 plots for same model :
termplot(model, main = paste("termplot( ", deparse(model$call)," ...)"))
termplot(model, rug = TRUE)
termplot(model, partial.resid = TRUE, se = TRUE, main = TRUE)
termplot(model, partial.resid = TRUE, smooth = panel.smooth, span.smth = 1/4)
if(!had.splines && rs) detach("package:splines")

if (require(MASS)) {
    hills.lm <- lm(log(time) ~ log(climb)+log(dist), data=hills)
    termplot(hills.lm, partial.resid = TRUE, smooth = panel.smooth,
             terms = "log(dist)", main = "Original")
    termplot(hills.lm, transform.x = TRUE, partial.resid = TRUE,
	     terms = "log(dist)", main = "Transformed",
	     smooth=panel.smooth)
}
}
\keyword{hplot}
\keyword{regression}
