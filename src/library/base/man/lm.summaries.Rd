\name{lm.summaries}
\title{Accessing Linear Model Fits}
\usage{
anova(object, \dots)
anovalist(object, \dots, test = NULL)
summary(object, correlation = FALSE)
coefficients(x) ; coef(x)
df.residual(x)
family(x)
formula(x)
fitted.values(x)
residuals(x)
weights(x)
plot(x)

print(summary.lm.obj, digits = max(3, .Options$digits - 3),
      symbolic.cor = p > 4,
      signif.stars= .Options$show.signif.stars, ...)
}
\alias{anova.lm}
\alias{anova.lm.null}
\alias{anova.mlm}
\alias{anovalist.lm}
\alias{summary.lm}
\alias{summary.lm.null}
\alias{summary.mlm}
\alias{coefficients.lm}
\alias{df.residual.lm}
\alias{family.lm}
\alias{formula.lm}
\alias{fitted.values.lm}
\alias{residuals.lm}
\alias{weights}
\alias{weights.lm}
\alias{weights.default}
\alias{plot.lm}
\alias{plot.mlm}
\alias{print.lm}
\alias{print.lm.null}
\alias{print.summary.lm}
\alias{print.summary.lm.null}
\arguments{
  \item{object,x}{an object of class \code{lm}, usually, a result of
    \code{\link{lm}(..)}.}
}
\description{
All these functions are \code{\link{methods}} for class \code{lm} or
\code{summary.lm} and \code{anova.lm} objects.
}
\details{
\code{print.summary.lm} tries to be smart about formatting the
coefficients, standard errors, etc. and additionally gives
``significance stars'' if \code{signif.stars} is \code{TRUE}.

\code{anova.lm} produces an analysis of variance (\code{anova}) table.

The generic accessor functions \code{coefficients}, \code{effects},
\code{fitted.values} and \code{residuals} can be used to extract
various useful features of the value returned by \code{lm}.
}
\value{
  The function \code{summary.lm} computes and returns a list of summary
  statistics of the fitted linear model given in \code{lm.obj}, using
  the slots (list elements) \code{"call"}, \code{"terms"}, and
  \code{"residuals"} from its argument, plus
  \item{coefficients}{a \eqn{p \times 4}{p x 4} matrix with columns for
    the estimated coefficient, its standard error, t-statistic and
    corresponding (two-sided) p-value.}
  \item{sigma}{the square root of the estimated variance of the random
    error
    \deqn{\hat\sigma^2 = \frac{1}{n-p}\sum_i{R_i^2},}{%
      sigma^2 = 1/(n-p) Sum(R[i]^2),}
    where \eqn{R_i}{R[i]} is the \eqn{i}-th residual,
    \code{residuals[i]}.}
  \item{df}{degrees of freedom, a 3-vector \eqn{(p,n-p,p*)} ...}
  \item{fstatistic}{a 3-vector with the value of the F-statistic with
    its numerator and denominator degrees of freedom.}
  \item{r.squared}{\eqn{R^2}, the ``fraction of variance explained by
    the model'',
    \deqn{R^2 = 1 - \frac{\sum_i{R_i^2}}{\sum_i(y_i- y^*)^2},}{%
      R^2 = 1 - Sum(R[i]^2) / Sum((y[i]- y*)^2),}
    where \eqn{y^*}{y*} is the mean of \eqn{y_i}{y[i]} if there is an
    intercept and zero otherwise.}
  \item{adj.r.squared}{the above \eqn{R^2} statistic
    \emph{``adjusted''}, penalizing for higher \eqn{p}.}
  \item{cov.unscaled}{a \eqn{p \times p}{p x p} matrix of (unscaled)
    covariances of the \eqn{\hat\beta_j}{coef..[j]}, \eqn{j=1,\dots,p}.}
  %%-- Rdconv just deletes the following line. This IS a bug !
  and if \code{correlation = TRUE} was specified,%- don't need anymore..
  \item{correlation}{the correlation matrix corresponding to the above
    \code{cov.unscaled}, if \code{correlation = TRUE} is specified.}
}
\seealso{
The model fitting function \code{\link{lm}}.

\code{\link{anova}} for the ANOVA table,
\code{\link{coefficients}}, \code{\link{deviance}},
\code{\link{effects}}, \code{\link{fitted.values}},
\code{\link{glm}} for \bold{generalized} linear models,
\code{\link{lm.influence}} for regression diagnostics,
\code{\link{residuals}}, \code{\link{summary}}.
}
\examples{
\testonly{example("lm", echo = FALSE)}
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
