\name{lm.summaries}
\alias{coefficients.lm}
\alias{family.lm}
\alias{formula.lm}
\alias{fitted.values.lm}
\alias{residuals.lm}
\alias{weights}
\alias{weights.lm}
\alias{weights.default}
\title{Accessing Linear Model Fits}
\usage{
\method{coefficients}{lm}(object, \dots) ; \method}{lm}{coef(object, \dots)
\method{family}{lm}(object, \dots)
\method{formula}{lm}(x, \dots)
\method{fitted.values}{lm}(object, \dots)
\method{residuals}{lm}(object,
          type=c("working","response", "deviance","pearson", "partial"), \dots)
weights(object, \dots)
}
\arguments{
  \item{object, x}{an object of class \code{lm}, usually, a result of a
    call to \code{\link{lm}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
All these functions are \code{\link{methods}} for class \code{"lm"}  objects.
}
\details{
The generic accessor functions \code{coefficients}, \code{effects},
\code{fitted.values} and \code{residuals} can be used to extract
various useful features of the value returned by \code{lm}.
}
\seealso{
  The model fitting function \code{\link{lm}}, \code{\link{anova.lm}}.

  \code{\link{coefficients}}, \code{\link{deviance}},
  \code{\link{df.residual}},
  \code{\link{effects}}, \code{\link{fitted.values}},
  \code{\link{glm}} for \bold{generalized} linear models,
  \code{\link{lm.influence}} for regression diagnostics,
  \code{\link{weighted.residuals}},
  \code{\link{residuals}}, \code{\link{residuals.glm}},
  \code{\link{summary.lm}}.
}
\examples{
\testonly{example("lm", echo = FALSE)}
##-- Continuing the  lm(.) example:
coef(lm.D90)# the bare coefficients

## The 2 basic regression diagnostic plots [plot.lm(.) is preferred]
plot(resid(lm.D90), fitted(lm.D90))# Tukey-Anscombe's
abline(h=0, lty=2, col = 'gray')

qqnorm(residuals(lm.D90))
}
\keyword{regression}
\keyword{models}
