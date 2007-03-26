\name{lm.summaries}
\alias{family.lm}
\alias{formula.lm}
\alias{residuals.lm}
\alias{labels.lm}
\alias{weights}
\alias{weights.default}
\title{Accessing Linear Model Fits}
\usage{
\method{family}{lm}(object, \dots)

\method{formula}{lm}(x, \dots)

\method{residuals}{lm}(object,
          type = c("working", "response", "deviance", "pearson",
                   "partial"),
          \dots)

\method{labels}{lm}(object, \dots)

weights(object, \dots)
}
\arguments{
  \item{object, x}{an object inheriting from class \code{lm}, usually
    the result of a call to \code{\link{lm}} or \code{\link{aov}}.}
  \item{\dots}{further arguments passed to or from other methods.}
  \item{type}{the type of residuals which should be returned.}
}
\description{
  All these functions are \code{\link{methods}} for class \code{"lm"}  objects.
}
\details{
  The generic accessor functions \code{coef}, \code{effects},
  \code{fitted} and \code{residuals} can be used to extract
  various useful features of the value returned by \code{lm}.

  The working and response residuals are \dQuote{observed - fitted}.  The
  deviance and pearson residuals are weighted residuals, scaled by the
  square root of the weights used in fitting.  The partial residuals
  are a matrix with each column formed by omitting a term from the
  model.  In all these, zero weight cases are never omitted (as opposed
  to the standardized \code{\link{rstudent}} residuals, and the
  \code{\link{weighted.residuals}}).

  How \code{residuals} treats cases with missing values in the original
  fit is determined by the \code{na.action} argument of that fit.
  If \code{na.action = na.omit} omitted cases will not appear in the
  residuals, whereas if \code{na.action = na.exclude} they will appear,
  with residual value \code{NA}.  See also \code{\link{naresid}}.

  The \code{"lm"} method for generic \code{\link{labels}} returns the
  term labels for estimable terms, that is the names of the terms with
  an least one estimable coefficient.
}
\seealso{
  The model fitting function \code{\link{lm}}, \code{\link{anova.lm}}.

  \code{\link{coef}}, \code{\link{deviance}},
  \code{\link{df.residual}},
  \code{\link{effects}}, \code{\link{fitted}},
  \code{\link{glm}} for \bold{generalized} linear models,
  \code{\link{influence}} (etc on that page) for regression diagnostics,
  \code{\link{weighted.residuals}},
  \code{\link{residuals}}, \code{\link{residuals.glm}},
  \code{\link{summary.lm}}.

  \link{influence.measures} for deletion diagnostics, including
  standardized (\code{\link{rstandard}})
  and studentized (\code{\link{rstudent}}) residuals.
}
\references{
  Chambers, J. M. (1992)
  \emph{Linear models.}
  Chapter 4 of \emph{Statistical Models in S}
  eds J. M. Chambers and T. J. Hastie, Wadsworth \& Brooks/Cole.
}
\examples{
\dontshow{example("lm", echo = FALSE)}
##-- Continuing the  lm(.) example:
coef(lm.D90)# the bare coefficients

## The 2 basic regression diagnostic plots [plot.lm(.) is preferred]
plot(resid(lm.D90), fitted(lm.D90))# Tukey-Anscombe's
abline(h=0, lty=2, col = 'gray')

qqnorm(residuals(lm.D90))
}
\keyword{regression}
\keyword{models}
