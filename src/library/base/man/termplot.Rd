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
         terms=NULL, se=FALSE, xlabs=NULL, ylab=NULL,
         colors=list(points=2,curve=1,rug=1,se=3), ...)
}
\arguments{
 \item{model}{A model object}
 \item{data}{data frame in which the variables in \code{model} can be found}
 \item{partial.resid}{Plot partial residuals?}
 \item{rug}{Add rugplots (jittered 1-d histograms) to the axes?}
 \item{terms}{Which terms to plot (default is all terms)}
 \item{se}{Plot plus/minus 2 standard error curves?}
 \item{xlabs}{Vector of labels for the x axes}
 \item{ylab}{Label for the y axes}
 \item{colors}{colors for the curve, partial residuals, rugplot and
   standard errors respectively. \code{colors=NULL} gives all black}
 \item{\dots}{Other graphical parameters}
}
\details{
  The model object must have a \code{predict} method that accepts
  \code{type=terms}, eg \code{\link{glm}} in the base package,
  \code{\link[survival5]{coxph}} and \code{\link[survival5]{survreg}} in the
  \code{survival5} package.

  For the \code{partial.resid=TRUE} option it must have a
  \code{\link{residuals}} method that accepts \code{type="partial"},
  currently only \code{\link{glm}} does.

  It is often necessary to specify the \code{data} argument, because it is
  not possible to reconstruct eg \code{x} from a model frame containing
  \code{sin(x)}.
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
termplot(model)
termplot(model, rug=T)
termplot(model, partial=T, rug=T,main=deparse(model$call))
termplot(model, partial=T, se = TRUE)
if(rs) detach("package:splines")
}
\keyword{hplot}
\keyword{regression}
