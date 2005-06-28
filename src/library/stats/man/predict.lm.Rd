\name{predict.lm}
\title{Predict method for Linear Model Fits}
\alias{predict.lm}
\alias{predict.mlm}
\concept{regression}
\description{
  Predicted values based on linear model object
}
\usage{
\method{predict}{lm}(object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
        interval = c("none", "confidence", "prediction"),
        level = 0.95, type = c("response", "terms"),
        terms = NULL, na.action = na.pass, \dots)
}
\arguments{
  \item{object}{Object of class inheriting from \code{"lm"}}
  \item{newdata}{An optional data frame in which to look for variables with
    which to predict.  If omitted, the fitted values are used.}
  \item{se.fit}{A switch indicating if standard errors are required.}
  \item{scale}{Scale parameter for std.err. calculation}
  \item{df}{Degrees of freedom for scale}
  \item{interval}{Type of interval calculation}
  \item{level}{Tolerance/confidence level}
  \item{type}{Type of prediction (response or model term)}
  \item{terms}{If \code{type="terms"}, which terms (default is all terms)}
  \item{na.action}{function determining what should be done with missing
    values in \code{newdata}.  The default is to predict \code{NA}.}
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
  data. That cannot be checked accurately, so a warning is issued.
}
\value{
  \code{predict.lm} produces a vector of predictions or a matrix of
  predictions and bounds with column names \code{fit}, \code{lwr}, and
  \code{upr} if \code{interval} is set.  If \code{se.fit} is
  \code{TRUE}, a list with the following components is returned: 
  \item{fit}{vector or matrix as above}
  \item{se.fit}{standard error of predicted means}
  \item{residual.scale}{residual standard deviations}
  \item{df}{degrees of freedom for residual}
}
\note{
  Variables are first looked for in \code{newdata} and then searched for
  in the usual way (which will include the environment of the formula
  used in the fit).  As from \R 2.0.0 a warning will be given if the
  variables found are not of the same length as those in \code{newdata}
  if it was supplied.
  
  Offsets specified by \code{offset} in the fit by \code{\link{lm}}
  will not be included in predictions, whereas those specified by an
  offset term in the formula will be.
}
\seealso{
  The model fitting function \code{\link{lm}}, \code{\link{predict}},
  \code{\link{SafePrediction}}
}
\examples{
## Predictions
x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
pred.w.plim <- predict(lm(y ~ x), new, interval="prediction")
pred.w.clim <- predict(lm(y ~ x), new, interval="confidence")
matplot(new$x,cbind(pred.w.clim, pred.w.plim[,-1]),
        lty=c(1,2,2,3,3), type="l", ylab="predicted y")
}
\keyword{regression}

