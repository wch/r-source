\name{influence.measures}
\title{Regression Deletion Diagnostics}
\alias{influence.measures}
\alias{print.infl}
\alias{summary.infl}
\alias{hat}
\alias{hatvalues}
\alias{hatvalues.lm}
\alias{rstandard}
\alias{rstandard.lm}
\alias{rstandard.glm}
\alias{rstudent}
\alias{rstudent.lm}
\alias{rstudent.glm}
\alias{dfbeta}
\alias{dfbeta.lm}
\alias{dfbetas}
\alias{dfbetas.lm}
\alias{dffits}
\alias{covratio}
\alias{cooks.distance}
\alias{cooks.distance.lm}
\alias{cooks.distance.glm}
\usage{
influence.measures(model)

rstandard(model, \dots)
\method{rstandard}{lm}(model, infl = lm.influence(model, do.coef = FALSE),
          sd = sqrt(deviance(model)/df.residual(model)), \dots)
\method{rstandard}{glm}(model, infl = lm.influence(model, do.coef = FALSE), \dots)

rstudent(model, \dots)
\method{rstudent}{lm}(model, infl = lm.influence(model, do.coef = FALSE),
	 res = infl$wt.res, \dots)
\method{rstudent}{glm}(model, infl = influence(model, do.coef = FALSE), \dots)

dffits(model, infl = , res = )

dfbeta(model, \dots)
\method{dfbeta}{lm}(model, infl = lm.influence(model, do.coef = TRUE), \dots)

dfbetas(model, \dots)
\method{dfbetas}{lm}(model, infl = lm.influence(model, do.coef = TRUE), \dots)

covratio(model, infl = lm.influence(model, do.coef = FALSE),
         res = weighted.residuals(model))

cooks.distance(model, \dots)
\method{cooks.distance}{lm}(model, infl = lm.influence(model, do.coef = FALSE),
               res = weighted.residuals(model),
               sd = sqrt(deviance(model)/df.residual(model)),
               hat = infl$hat, \dots)
\method{cooks.distance}{glm}(model, infl = influence(model, do.coef = FALSE),
               res = infl$pear.res,
               dispersion = summary(model)$dispersion,
               hat = infl$hat, \dots)

hatvalues(model, \dots)
\method{hatvalues}{lm}(model, infl = lm.influence(model, do.coef = FALSE), \dots)

hat(x, intercept = TRUE)
}
\arguments{
  \item{model}{an \R object, typically returned by \code{\link{lm}} or
    \code{\link{glm}}.}
  \item{infl}{influence structure as returned by
    \code{\link{lm.influence}} or \code{\link{influence}} (the latter
    only for the \code{glm} method of \code{rstudent} and
    \code{cooks.distance}).}
  \item{res}{(possibly weighted) residuals, with proper default.}
  \item{sd}{standard deviation to use, see default.}
  \item{dispersion}{dispersion (for \code{\link{glm}} objects) to use,
    see default.}
  \item{hat}{hat values \eqn{H_{ii}}{H[i,i]}, see default.}

  \item{x}{the \eqn{X} or design matrix.}
  \item{intercept}{should an intercept column be pre-prended to \code{x}?}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  This suite of functions can be used to compute some of the regression
  (leave-one-out deletion) diagnostics for linear and generalized linear
  models discussed in Belsley, Kuh and Welsch (1980), Cook and Weisberg (1982),
  etc.
}
\details{
  The primary high-level function is \code{influence.measures} which produces a
  class \code{"infl"} object tabular display showing the DFBETAS for
  each model variable, DFFITS, covariance ratios, Cook's distances and
  the diagonal elements of the hat matrix.  Cases which are influential
  with respect to any of these measures are marked with an asterisk.

  The functions \code{dfbetas}, \code{dffits},
  \code{covratio} and \code{cooks.distance} provide direct access to the
  corresponding diagnostic quantities.  Functions \code{rstandard} and
  \code{rstudent} give the standardized and Studentized residuals
  respectively. (These re-normalize the residuals to have unit variance,
  using an overall and leave-one-out measure of the error variance
  respectively.)

  Values for generalized linear models are approximations, as described
  in Williams (1987) (except that Cook's distances are scaled as
  \eqn{F} rather than as chi-square values).

  The optional \code{infl}, \code{res} and \code{sd} arguments are there
  to encourage the use of these direct access functions, in situations
  where, e.g., the underlying basic influence measures (from
  \code{\link{lm.influence}} or the generic \code{\link{influence}}) are
  already available.

  Note that cases with \code{weights == 0} are \emph{dropped} from all
  these functions, but that if a linear model has been fitted with
  \code{na.action = na.exclude}, suitable values are filled in for the
  cases excluded during fitting.

  The function \code{hat()} exists mainly for S (version 2)
  compatibility; we recommend using \code{hatvalues()} instead.
}
\note{
  For \code{hatvalues}, \code{dfbeta}, and \code{dfbetas}, the method
  for linear models also works for generalized linear models.
}
\author{
  Several R core team members and John Fox, originally in his \file{car}
  package.
}
\references{
  Belsley, D. A., Kuh, E. and Welsch, R. E. (1980)
  \emph{Regression Diagnostics}.
  New York: Wiley.

  Cook, R. D. and Weisberg, S. (1982)
  \emph{Residuals and Influence in Regression}.
  London: Chapman and Hall.

  Williams, D. A. (1987)
  Generalized linear model diagnostics using the deviance and single
  case deletions. \emph{Applied Statistics} \bold{36}, 181--191.

  Fox, J. (1997)
  \emph{Applied Regression, Linear Models, and Related Methods}. Sage.

  Fox, J. (2002)
  \emph{An R and S-Plus Companion to Applied Regression}.
  Sage Publ.; \url{http://www.socsci.mcmaster.ca/jfox/Books/Companion/}.
}
\seealso{\code{\link{influence}} (containing \code{\link{lm.influence}}).
}
\examples{
## Analysis of the life-cycle savings data
## given in Belsley, Kuh and Welsch.
lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)

inflm.SR <- influence.measures(lm.SR)
which(apply(inflm.SR$is.inf, 1, any)) # which observations 'are' influential
summary(inflm.SR) # only these
inflm.SR          # all
plot(rstudent(lm.SR) ~ hatvalues(lm.SR)) # recommended by some

## The 'infl' argument is not needed, but avoids recomputation:
rs <- rstandard(lm.SR)
iflSR <- influence(lm.SR)
identical(rs, rstandard(lm.SR, infl = iflSR))
## to "see" the larger values:
1000 * round(dfbetas(lm.SR, infl = iflSR), 3)

## Huber's data [Atkinson 1985]
xh <- c(-4:0, 10)
yh <- c(2.48, .73, -.04, -1.44, -1.32, 0)
summary(lmH <- lm(yh ~ xh))
(im <- influence.measures(lmH))
plot(xh,yh, main = "Huber's data: L.S. line and influential obs.")
abline(lmH); points(xh[im$is.inf], yh[im$is.inf], pch=20, col=2)
}
\keyword{regression}
