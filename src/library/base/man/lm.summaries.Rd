\name{lm summaries}
\title{Accessing Linear Model Fits}
\usage{
anova(lm.obj, \dots)
summary(lm.obj, correlation = FALSE)
coefficients(lm.obj)
deviance(lm.obj)
df.residual(lm.obj)
effects(lm.obj)
fitted.values(lm.obj)
residuals(lm.obj)
weights(lm.obj)
}
\alias{anova.lm}
\alias{summary.lm}
\alias{coefficients.lm}
\alias{deviance.lm}
\alias{df.residual.lm}
\alias{effects.lm}
\alias{fitted.values.lm}
\alias{residuals.lm}
\alias{weights.lm}
\alias{print.lm}
\alias{print.summary.lm}
\alias{print.anova.lm}
\arguments{
  \item{lm.obj}{an object of class \code{lm}, usually, a result of
    \code{\link{lm}(..)}.}
}
\description{
All these functions are \code{\link{methods}} for class \code{lm} objects.
}
\value{
The function \code{summary.lm} computes
summary statistics of the fitted linear model
 of the results and the function \code{anova}
and be used to produce and analysis of variance table.
The generic accessor functions \code{coefficients},
\code{effects}, \code{fitted.values} and \code{residuals}
can be used to extract various useful features of the
value returned by \code{lm}.
}
\seealso{
The model fitting function \code{\link{lm}}.

\code{\link{anova}} for the ANOVA table,
\code{\link{coefficients}}, \code{\link{effects}}, \code{\link{fitted.values}},
\code{\link{glm}} for \bold{generalized} linear models,
\code{\link{lm.influence}} for regression diagnostics,
\code{\link{residuals}}, \code{\link{summary}}.
}
\examples{
##-- Continuing the  lm(.) example:
coef(lm.D90)# the bare coefficients
sld90 <- summary(lm.D90 <- lm(weight ~ group -1))# omitting intercept
sld90
coef(sld90)# much more 

## The 2 basic regression diagnostic plots:
plot(resid(lm.D90), fitted(lm.D90))# Tukey-Anscombe's
abline(h=0, lty=2, col = 'gray')

qqnorm(residuals(lm.D90))
}
\keyword{regression}
