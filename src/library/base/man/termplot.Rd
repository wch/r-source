\name{termplot}
\alias{termplot}
\alias{plot.gam}
\title{Plot regression terms}
\description{
Plots regression terms against their predictors, optionally with
standard errors and partial residuals added. 
}
\usage{
termplot(model, data=model.frame(model), partial.resid=FALSE, rug=FALSE, terms=NULL, se=FALSE, xlabs=NULL, ylab=NULL, ...)
}
%- maybe also `usage' for other objects documented here.
\arguments{
 \item{model}{A model object}
 \item{data}{data frame in which the variables in \code{model} can be found}
 \item{partial.resid}{Plot partial residuals?}
 \item{rug}{Add rugplots (jittered 1-d histograms) to the axes?}
 \item{terms}{Which terms to plot (default is all terms)}
 \item{se}{Plot pointwise standard errors?}
 \item{xlabs}{Vector of labels for the x axes}
 \item{ylab}{Label for the y axes}
 \item{\dots}{Other graphical parameters}
}
\details{
The model object must have a \code{predict} method that accepts
\code{type=terms}, eg \code{\link{glm}} in the base package,
\code{coxph} and \code{survreg} in the \code{survival5} package.

For the \code{partial.resid=TRUE} option it must have a \code{residuals}
method that accepts \code{type="partial"}, currently only
\code{\link{glm}} does.

It is often necessary to specify the \code{data} argument, because it is
not possible to reconstruct eg \code{x} from a model frame containing
\code{sin(x)}.
 }


\examples{
library(splines)
x<-1:100
z<-factor(rep(1:4,25))
y<-rnorm(100,sin(x/10)+as.numeric(z))
model<-glm(y~ns(x,6)+z)
par(mfrow=c(3,2))
termplot(model)
termplot(model,rug=T)
termplot(model,partial=T,rug=T)
detach("package:splines")
}
\keyword{hplot}%-- one or more ...
\keyword{regression}%-- one or more ...
