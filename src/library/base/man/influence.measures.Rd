\name{influence.measures}
\title{Regression Diagnostics}
\usage{
influence.measures(lm.obj)
summary.infl (object, digits = max(2, getOption("digits") - 5), \dots)
  print.infl (x, digits = max(3, getOption("digits") - 4), \dots)

rstandard(lm.obj,
          infl = lm.influence(lm.obj),
          res = weighted.residuals(lm.obj),
          sd = sqrt(deviance(lm.obj)/df.residual(lm.obj)))
rstudent (lm.obj, infl = \dots, res = \dots)
dffits   (lm.obj, infl = \dots, res = \dots)
dfbetas  (lm.obj, infl = \dots)
covratio (lm.obj, infl = \dots, res = \dots)
cooks.distance(lm.obj, infl = \dots, res = \dots, sd = \dots)

hat(x, intercept = TRUE)
}
\alias{influence.measures}
\alias{print.infl}
\alias{summary.infl}

\alias{hat}
\alias{rstandard}
\alias{rstudent}
\alias{dfbetas}
\alias{dffits}
\alias{covratio}
\alias{cooks.distance}
\arguments{
  \item{lm.obj}{the resulting object returned by \code{\link{lm}}.}

  \item{infl}{influence structure as returned by \code{\link{lm.influence}}.}
  \item{res}{(possibly weighted) residuals, with proper default.}
  \item{sd}{standard deviation to use, see default.}

  \item{x}{the `X' or design matrix.}
  \item{intercept}{should an intercept column be pre-prended to \code{x}?}
}
\description{
  This suite of functions can be used to compute some of the regression
  diagnostics discussed in Belsley, Kuh and Welsch (1980), and in Cook
  and Weisberg (1982).
}
\details{
  The primary function is \code{influence.measures} which produces a
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

  The optional \code{infl}, \code{res} and \code{sd} arguments are there
  to encourage the use of these direct access functions, in situations
  where, e.g., the underlying basic influence measures (from
  \code{\link{lm.influence}}) are already available.

  Note that cases with \code{weights == 0} are \emph{dropped} from all
  these functions.
}
\references{
  Belsley, D. A., Kuh, E. and Welsch, R. E. (1980)
  \emph{Regression Diagnostics.}
  New York: Wiley.

  Cook, R. D. and Weisberg, S. (1982)
  \emph{Residuals and Influence in Regression.}
  London: Chapman and Hall.
}
\seealso{\code{\link{lm.influence}}.
}
\examples{
## Analysis of the life-cycle savings data
## given in Belsley, Kuh and Welsch.
data(LifeCycleSavings)
lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings)
summary(inflm.SR <- influence.measures(lm.SR))
inflm.SR
which(apply(inflm.SR$is.inf, 1, any)) # which observations `are' influential
dim(dfb <- dfbetas(lm.SR))            # the 1st columns of influence.measures
all(dfb == inflm.SR$infmat[, 1:5])
rstandard(lm.SR)
rstudent(lm.SR)
dffits(lm.SR)
covratio(lm.SR)

## Huber's data [Atkinson 1985]
xh <- c(-4:0, 10)
yh <- c(2.48, .73, -.04, -1.44, -1.32, 0)
summary(lmH <- lm(yh ~ xh))
influence.measures(lmH)
}
\keyword{regression}
