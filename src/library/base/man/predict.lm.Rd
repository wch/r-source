\name{predict.lm}
\title{Predicting from Linear Model Fits}
\usage{
predict(object, newdata = model.frame(object), se.fit = FALSE,  
  scale = NULL, df = Inf, 
  interval = c("none", "confidence", "prediction"), level = 0.95)
}

\description{
\code{predict.lm} produces predicted values, obtained by evaluating
the regression function in the frame \code{newdata}. If the logical
\code{se.fit} is \code{TRUE}, standard errors of the predictions are
calculated. If the numeric argument \code{scale} is set (with optional
\code{df}), it is used as the residual standard deviation in the
computation of the standard errors, otherwise this is extracted from
the model fit. Setting \code{intervals} specifies computation of
confidence or prediction (tolerance) intervals at the specified
\code{level}.
}
\value{
  \code{predict.lm} produces a vector of predictions or a matrix of
  predictions and bounds with column names \code{fit}, \code{lwr}, and
  \code{upr} if \code{interval} is set. If \code{se.fit} is
  \code{TRUE}, a list with the following components is returned

  \item{fit}{vector or matrix as above}
  \item{se.fit}{standard error of predictions}
  \item{residual.scale}{residual standard deviations}
  \item{df}{degrees of freedom for residual}
}

\seealso{
The model fitting function \code{\link{lm}}

\code{\link{predict}}}

\examples{
## Predictions
x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x))
predict(lm(y ~ x), data.frame(x = seq(-3, 3, 0.1)), se = TRUE)

}
\keyword{regression}




