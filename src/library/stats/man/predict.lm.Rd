% File src/library/stats/man/predict.lm.Rd
% Part of the R package, https://www.R-project.org
% Copyright 1995-2023 R Core Team
% Distributed under GPL 2 or later

\name{predict.lm}
\title{Predict method for Linear Model Fits}
\alias{predict.lm}
%\alias{predict.mlm}
\concept{regression}
\description{
  Predicted values based on linear model object.
}
\usage{
\method{predict}{lm}(object, newdata, se.fit = FALSE, scale = NULL, df = Inf,
        interval = c("none", "confidence", "prediction"),
        level = 0.95, type = c("response", "terms"),
        terms = NULL, na.action = na.pass,
        pred.var = res.var/weights, weights = 1,
        rankdeficient = c("warnif", "simple", "non-estim", "NA", "NAwarn"),
        tol = 1e-6, verbose = FALSE,
        \dots)
}
\arguments{
  \item{object}{Object of class inheriting from \code{"lm"}}
  \item{newdata}{An optional data frame in which to look for variables with
    which to predict.  If omitted, the fitted values are used.}
  \item{se.fit}{A switch indicating if standard errors are required.}
  \item{scale}{Scale parameter for std.err. calculation.}
  \item{df}{Degrees of freedom for scale.}
  \item{interval}{Type of interval calculation.  Can be abbreviated.}
  \item{level}{Tolerance/confidence level.}
  \item{type}{Type of prediction (response or model term).  Can be abbreviated.}
  \item{terms}{If \code{type = "terms"}, which terms (default is all
    terms), a \code{\link{character}} vector.}
  \item{na.action}{function determining what should be done with missing
    values in \code{newdata}.  The default is to predict \code{NA}.}
  \item{pred.var}{the variance(s) for future observations to be assumed
    for prediction intervals.  See \sQuote{Details}.}
  \item{weights}{variance weights for prediction.  This can be a numeric
    vector or a one-sided model formula.  In the latter case, it is
    interpreted as an expression evaluated in \code{newdata}.}
  \item{rankdeficient}{a \code{\link{character}} string specifying what
    should happen in the case of a rank deficient model, i.e., when
    \code{object$rank < ncol(model.matrix(object))}.
    \describe{
      \item{\code{"warnif"}:}{gives a \code{\link{warning}} only in case of
	predicting \sQuote{non-estimable} cases, i.e., vectors not in the
	same predictor subspace as the original data (with tolerance
	\code{tol}).  In that case, the non-estimable indices are also
	returned as attribute \code{"non-estim"} (see \code{rankdeficient="non-estim"}).}
      \item{\code{"simple"}:}{is back compatible to \R < 4.3.0, possibly giving dubious
	predictions in non-estimable cases, and always signalling a \code{\link{warning}}.}
      \item{\code{"non-estim"}:}{gives the same predictions without
	\code{\link{warning}}, and with an attribute \code{\link{attr}(*, "non-estim")}
	with indices in \code{1:nrow(newdata)} of new data observations
	which are deemed non-estimable.}
      \item{\code{"NA"}:    }{predicts \code{NA} for non-estimable new data,
	silently.  Often recommended in new code.}
      \item{\code{"NAwarn"}:}{predicts \code{NA} for non-estimable new data
	with a \code{\link{warning}}.}
    }
  }
  \item{tol}{non-negative number determining how non-estimability is
    determined in rank deficient cases.}
  \item{verbose}{\code{\link{logical}} indicating if messages should be
    produced about rank deficiency handling.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\details{
  \code{predict.lm} produces predicted values, obtained by evaluating
  the regression function in the frame \code{newdata} (which defaults to
  \code{model.frame(object)}).  If the logical \code{se.fit} is
  \code{TRUE}, standard errors of the predictions are calculated.  If
  the numeric argument \code{scale} is set (with optional \code{df}), it
  is used as the residual standard deviation in the computation of the
  standard errors, otherwise this is extracted from the model fit.
  Setting \code{intervals} specifies computation of confidence or
  prediction (tolerance) intervals at the specified \code{level}, sometimes
  referred to as narrow vs. wide intervals.

  If the fit is rank-deficient, some of the columns of the design matrix
  will have been dropped during the \code{\link{lm}} computations, and
  corresponding \code{\link{coef}()} components set to \code{\link{NA}}.
  Prediction from such a fit only makes sense if \code{newdata} is
  contained in the same subspace as the original data.  Other
  \code{newdata} entries (rows) are \code{non-estimable}.  This is now
  checked (up to numerical tolerance \code{tol}) unless \code{rankdeficient
    == "simple"}, which corresponds to previous behaviour, warns always and
  predicts using the non-\code{NA} coefficients with the corresponding
  columns of the design matrix.  The new default option,
  \code{rankdeficient == "warnif"} checks if there are
  \dQuote{non-estimable} cases (up to tolerance \code{tol}) and only warns
  in that case.  All further \code{rankdeficient} options also check and
  either predict \code{NA} or mark the non-estimable cases differently.

  If \code{newdata} is omitted the predictions are based on the data
  used for the fit.  In that case how cases with missing values in the
  original fit are handled is determined by the \code{na.action} argument of that
  fit.  If \code{na.action = na.omit} omitted cases will not appear in
  the predictions, whereas if \code{na.action = na.exclude} they will
  appear (in predictions, standard errors or interval limits),
  with value \code{NA}.  See also \code{\link{napredict}}.

  The prediction intervals are for a single observation at each case in
  \code{newdata} (or by default, the data used for the fit) with error
  variance(s) \code{pred.var}.  This can be a multiple of \code{res.var},
  the estimated value of \eqn{\sigma^2}: the default is to assume that
  future observations have the same error variance as those
  used for fitting.  If \code{weights} is supplied, the inverse of this
  is used as a scale factor.  For a weighted fit, if the prediction
  is for the original data frame, \code{weights} defaults to the weights
  used for the  model fit, with a warning since it might not be the
  intended result.  If the fit was weighted and \code{newdata} is given, the
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
  predictors as used for the fit.  The variance of the \emph{residuals}
  will be smaller.

  Strictly speaking, the formula used for prediction limits assumes that
  the degrees of freedom for the fit are the same as those for the
  residual variance.  This may not be the case if \code{res.var} is
  not obtained from the fit.
}
\seealso{
  The model fitting function \code{\link{lm}}, \code{\link{predict}}.

  \link{SafePrediction} for prediction from (univariable) polynomial and
  spline fits.
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
                    tolerance = 1e-12, check.attributes = FALSE))
}
\keyword{regression}
