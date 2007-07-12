\name{termplot}
\alias{termplot}
\title{Plot regression terms}
\description{
  Plots regression terms against their predictors, optionally with
  standard errors and partial residuals added.
}
\usage{
termplot(model, data = NULL, envir = environment(formula(model)),
         partial.resid = FALSE, rug = FALSE,
         terms = NULL, se = FALSE, xlabs = NULL, ylabs = NULL, main = NULL,
         col.term = 2, lwd.term = 1.5,
         col.se = "orange", lty.se = 2, lwd.se = 1,
         col.res = "gray", cex = 1, pch = par("pch"),
         col.smth = "darkred", lty.smth = 2, span.smth = 2/3,
         ask = dev.interactive() && nb.fig < n.tms,
         use.factor.levels = TRUE, smooth = NULL, ylim = "common", \dots)
}
\arguments{
  \item{model}{fitted model object}
  \item{data}{data frame in which variables in \code{model} can be
    found}
  \item{envir}{environment in which variables in \code{model} can be found}
  \item{partial.resid}{logical; should partial residuals be plotted?}
  \item{rug}{add \link{rug}plots (jittered 1-d histograms) to the axes?}
  \item{terms}{which terms to plot (default \code{NULL} means all terms)}
  \item{se}{plot pointwise standard errors?}
  \item{xlabs}{vector of labels for the x axes}
  \item{ylabs}{vector of labels for the y axes}
  \item{main}{logical, or vector of main titles;  if \code{TRUE}, the
    model's call is taken as main title, \code{NULL} or \code{FALSE} mean
    no titles.}
  \item{col.term, lwd.term}{color and line width for the \dQuote{term curve},
    see \code{\link{lines}}.}
  \item{col.se, lty.se, lwd.se}{color, line type and line width for the
    \dQuote{twice-standard-error curve} when \code{se = TRUE}.}
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
   \item{\dots}{other graphical parameters.}
}
\details{
  The model object must have a \code{predict} method that accepts
  \code{type=terms}, eg \code{\link{glm}} in the \pkg{base} package,
  \code{\link[survival]{coxph}} and \code{\link[survival]{survreg}} in
  the \pkg{survival} package.

  For the \code{partial.resid=TRUE} option it must have a
  \code{\link{residuals}} method that accepts \code{type="partial"},
  which \code{\link{lm}} and \code{\link{glm}} do.

  The \code{data} argument should rarely be needed, but in some cases
  \code{termplot} may be unable to reconstruct the original data
  frame. Using \code{na.action=na.exclude} makes these problems less likely.

  Nothing sensible happens for interaction terms.
}
\seealso{For (generalized) linear models, \code{\link{plot.lm}} and
  \code{\link{predict.glm}}.}
\examples{
require(graphics)

had.splines <- "package:splines" \%in\% search()
if(!had.splines) rs <- require(splines)
x <- 1:100
z <- factor(rep(LETTERS[1:4],25))
y <- rnorm(100, sin(x/10)+as.numeric(z))
model <- glm(y ~ ns(x,6) + z)

par(mfrow=c(2,2)) ## 2 x 2 plots for same model :
termplot(model, main = paste("termplot( ", deparse(model$call)," ...)"))
termplot(model, rug=TRUE)
termplot(model, partial.resid=TRUE, se = TRUE, main = TRUE)
termplot(model, partial.resid=TRUE, smooth=panel.smooth, span.smth=1/4)
if(!had.splines && rs) detach("package:splines")
}
\keyword{hplot}
\keyword{regression}
