% File src/library/stats/man/predict.lm.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2013 R Core Team
% Distributed under GPL 2 or later

\name{predict.lm}
\title{Predict method for Linear Model Fits}
\alias{predict.lm}
\alias{predict.mlm}
\concept{regression}
\description{
  Predicted values based on linear model object.
}
\usage{
\method{predict}{lm}(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
        interval = c("none", "confidence", "prediction"),
        level = 0.95, type = c("response", "terms"),
        terms = NULL, na.action = na.pass,
        pred.var = res.var/weights, weights = 1, \dots)
}
\arguments{
  \item{object}{Object of class inheriting from \code{"lm"}}
  \item{newdata}{An optional data frame in which to look for variables with
    which to predict.  If omitted, the fitted values are used.}
  \item{se.fit}{A switch indicating if standard errors are required.}
  \item{scale}{Scale parameter for std.err. calculation}
  \item{df}{Degrees of freedom for scale}
  \item{interval}{Type of interval calculation.}
  \item{level}{Tolerance/confidence level}
  \item{type}{Type of prediction (response or model term).}
  \item{terms}{If \code{type = "terms"}, which terms (default is all
    terms), a \code{\link{character}} vector.}
  \item{na.action}{function determining what should be done with missing
    values in \code{newdata}.  The default is to predict \code{NA}.}
  \item{pred.var}{the variance(s) for future observations to be assumed
    for prediction intervals.  See \sQuote{Details}.}
  \item{weights}{variance weights for prediction. This can be a numeric
    vector or a one-sided model formula. In the latter case, it is
    interpreted as an expression evaluated in \code{newdata}}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{predict.lm} produces predicted values, obtained by evaluating
  the regression function in the frame \code{newdata} (which defaults to
  \code{model.frame(object)}.  If the logical \code{se.fit} is
  \code{TRUE}, standard errors of the predictions are calculated.  If
  the numeric argument \code{scale} is set (with optional \code{df}), it
  is used as the residual standard deviation in the computation of the
  standard errors, otherwise this is extracted from the model fit.
  Setting \code{intervals} specifies computation of confidence or
  prediction (tolerance) intervals at the specified \code{level}, sometimes
  referred to as narrow vs. wide intervals.

  If the fit is rank-deficient, some of the columns of the design matrix
  will have been dropped.  Prediction from such a fit only makes sense
  if \code{newdata} is contained in the same subspace as the original
  data.  That cannot be checked accurately, so a warning is issued.

  If \code{newdata} is omitted the predictions are based on the data
  used for the fit.  In that case how cases with missing values in the
  original fit are handled is determined by the \code{na.action} argument of that
  fit.  If \code{na.action = na.omit} omitted cases will not appear in
  the predictions, whereas if \code{na.action = na.exclude} they will
  appear (in predictions, standard errors or interval limits),
  with value \code{NA}.  See also \code{\link{napredict}}.

  The prediction intervals are for a single observation at each case in
  \code{newdata} (or by default, the data used for the fit) with error
  variance(s) \code{pred.var}. This can be a multiple of \code{res.var},
  the estimated
  value of \eqn{\sigma^2}: the default is to assume that future
  observations have the same error variance as those
  used for fitting. If \code{weights} is supplied, the inverse of this
  is used as a scale factor. For a weighted fit, if the prediction
  is for the original data frame, \code{weights} defaults to the weights
  used for the  model fit, with a warning since it might not be the
  intended result. If the fit was weighted and \code{newdata} is given, the
  default is to assume constant prediction variance, with a warning.
}
\value{
  \code{predict.lm} produces a vector of predictions or a matrix of
  predictions and bounds with column names \code{fit}, \code{lwr}, and
  \code{upr} if \code{interval} is set.  For \code{type = "terms"} this
  is a matrix with a column per term and may have an attribute
  \code{"constant"}.

  If \code{se.fit} is
  \code{TRUE}, a list with the following components is returned:
  \item{fit}{vector or matrix as above}
  \item{se.fit}{standard error of predicted means}
  \item{residual.scale}{residual standard deviations}
  \item{df}{degrees of freedom for residual}
}
\note{
  Variables are first looked for in \code{newdata} and then searched for
  in the usual way (which will include the environment of the formula
  used in the fit).  A warning will be given if the
  variables found are not of the same length as those in \code{newdata}
  if it was supplied.

  Notice that prediction variances and prediction intervals always refer
  to \emph{future} observations, possibly corresponding to the same
  predictors as used for the fit. The variance of the \emph{residuals}
  will be smaller.

  Strictly speaking, the formula used for prediction limits assumes that
  the degrees of freedom for the fit are the same as those for the
  residual variance.  This may not be the case if \code{res.var} is
  not obtained from the fit.
}
\seealso{
  The model fitting function \code{\link{lm}}, \code{\link{predict}}.

  \link{SafePrediction} for prediction from polynomial and spline fits.
}
\examples{
require(graphics)

## Predictions
x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval = "prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval = "confidence")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), type = "l", ylab = "predicted y")

## Prediction intervals, special cases
##  The first three of these throw warnings
w <- 1 + x^2
fit <- lm(y ~ x)
wfit <- lm(y ~ x, weights = w)
predict(fit, interval = "prediction")
predict(wfit, interval = "prediction")
predict(wfit, new, interval = "prediction")
predict(wfit, new, interval = "prediction", weights = (new$x)^2)
predict(wfit, new, interval = "prediction", weights = ~x^2)

##-- From  aov(.) example ---- predict(.. terms)
npk.aov <- aov(yield ~ block + N*P*K, npk)
(termL <- attr(terms(npk.aov), "term.labels"))
(pt <- predict(npk.aov, type = "terms"))
pt. <- predict(npk.aov, type = "terms", terms = termL[1:4])
stopifnot(all.equal(pt[,1:4], pt.,
                    check.attr = FALSE, tol = 1e-12))
}
\keyword{regression}
