\name{termplot}
\alias{termplot}
\alias{plot.gam}
\title{Plot regression terms}
\description{
  Plots regression terms against their predictors, optionally with
  standard errors and partial residuals added.
}
\usage{
termplot(model, data=model.frame(model), partial.resid=FALSE, rug=FALSE,
         terms=NULL, se=FALSE, xlabs=NULL, ylabs=NULL, main = NULL,
         col.term = 2, lwd.term = 1.5,
         col.se = "orange", lty.se = 2, lwd.se = 2,
         col.res= "gray", cex = 1, pch = par("pch"),
         ask = interactive() && nb.fig < n.tms && .Device != "postscript",
         ...)
}
\arguments{
  \item{model}{fitted model object}
  \item{data}{data frame in which the variables in \code{model} can be found}
  \item{partial.resid}{logical; should partial residuals be plotted?}
  \item{rug}{add \link{rug}plots (jittered 1-d histograms) to the axes?}
  \item{terms}{which terms to plot (default \code{NULL} means all terms)}
  \item{se}{plot pointwise standard errors?}
  \item{xlabs}{vector of labels for the x axes}
  \item{ylabs}{vector of labels for the y axes}
  \item{main}{logical, or vector of main titles;  if \code{TRUE}, the
    model's call is taken as main title, \code{NULL} or \code{FALSE} mean
    no titles.}
  \item{col.term, lwd.term}{color and line width for the ``term curve'',
    see \code{\link{lines}}.}
  \item{col.se, lty.se, lwd.se}{color, line type and line width for the
    ``twice-standard-error curve'' when \code{se = TRUE}.}
  \item{col.res, cex, pch}{color, plotting character expansion and type
    for partial residuals, when \code{partial.resid = TRUE}, see
    \code{\link{points}}.}
   \item{ask}{logical; if \code{TRUE}, the user is \emph{ask}ed before
     each plot, see \code{\link{par}(ask=.)}.}
  \item{\dots}{other graphical parameters}
}
\details{
  The model object must have a \code{predict} method that accepts
  \code{type=terms}, eg \code{\link{glm}} in the base package,
  \code{\link[survival5]{coxph}} and \code{\link[survival5]{survreg}} in the
  \code{survival5} package.

  For the \code{partial.resid=TRUE} option it must have a
  \code{\link{residuals}} method that accepts \code{type="partial"},
  which \code{\link{lm}} and \code{\link{glm}} do.

  It is often necessary to specify the \code{data} argument, because it is
  not possible to reconstruct eg \code{x} from a model frame containing
  \code{sin(x)}.  The \code{data} argument must have exactly the same
  rows as the model frame of the model object so, for example, missing
  data must  have been removed in the same way.
}
\seealso{For (generalized) linear models, \code{\link{plot.lm}} and
  \code{\link{predict.glm}}.}
\examples{
rs <- require(splines)
x <- 1:100
z <- factor(rep(1:4,25))
y <- rnorm(100,sin(x/10)+as.numeric(z))
model <- glm(y ~ ns(x,6) + z)

par(mfrow=c(2,2)) ## 2 x 2 plots for same model :
termplot(model, main = paste("termplot( ", deparse(model$call)," ..)"))
termplot(model, rug=TRUE)
termplot(model, partial=TRUE, rug= TRUE,
         main="termplot(..,partial = T, rug = T)")
termplot(model, partial=TRUE, se = TRUE, main = TRUE)
if(rs) detach("package:splines")
}
\keyword{hplot}
\keyword{regression}
