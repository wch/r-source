\name{predict.lm}
\title{Predicting from Linear Model Fits}
\alias{predict.lm}
\alias{predict.mlm}
\description{
  Obtains predictions and optionally estimates standard errors of those
  predictions from a fitted linear model object.}
\usage{
predict[.lm](object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
             interval = c("none", "confidence", "prediction"),
             level = 0.95, type = c("response", "terms"),
             terms = NULL, \dots)
}
\arguments{
  \item{object}{a fitted object of class inheriting from \code{"lm"}.}
  \item{newdata}{}
  \item{se.fit}{A logical indicating whether or not to compute pointwise
    standard errors along with the predictions.}
  \item{scale}{}
  \item{df}{}
  \item{interval}{}
  \item{level}{}
  \item{type}{the type of prediction required.  The default is on the
    scale of the response variable.  The alternative \code{"terms"}
    breaks predictions down to the contributions from each term, and
    returns a matrix of predictions, with columns corresponding to the
    terms of the model.}
  \item{terms}{}
  \item{\dots}{not used.}
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
  prediction (tolerance) intervals at the specified \code{level}.
}
\value{
  \code{predict.lm} produces a vector of predictions or a matrix of
  predictions and bounds with column names \code{fit}, \code{lwr}, and
  \code{upr} if \code{interval} is set.  If \code{se.fit} is
  \code{TRUE}, a list with the following components is returned: 

  \item{fit}{vector or matrix as above}
  \item{se.fit}{standard error of predictions}
  \item{residual.scale}{residual standard deviations}
  \item{df}{degrees of freedom for residual}
}
\seealso{
  The model fitting function \code{\link{lm}}, \code{\link{predict}}.
}
\examples{
## Predictions
x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
predict(lm(y ~ x), data.frame(x = seq(-3, 3, 0.1)), se = TRUE)
}
\keyword{regression}
