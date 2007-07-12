\name{predict.glm}
\alias{predict.glm}
\concept{regression}
\title{Predict Method for GLM Fits}
\usage{
\method{predict}{glm}(object, newdata = NULL,
            type = c("link", "response", "terms"),
            se.fit = FALSE, dispersion = NULL, terms = NULL,
            na.action = na.pass, \dots)
}
\arguments{
  \item{object}{a fitted object of class inheriting from \code{"glm"}.}
  \item{newdata}{optionally, a data frame in which to look for variables with
    which to predict.  If omitted, the fitted linear predictors are used.}
  \item{type}{the type of prediction required.  The default is on the
    scale of the linear predictors; the alternative \code{"response"}
    is on the scale of the response variable.  Thus for a default
    binomial model the default predictions are of log-odds (probabilities
    on logit scale) and \code{type = "response"} gives the predicted
    probabilities.  The \code{"terms"} option returns a matrix giving the
    fitted values of each term in the model formula on the linear predictor
    scale.

    The value of this argument can be abbreviated.
  }
  \item{se.fit}{logical switch indicating if standard errors are required.}
  \item{dispersion}{the dispersion of the GLM fit to be assumed in
    computing the standard errors.  If omitted, that returned by
    \code{summary} applied to the object is used.}
  \item{terms}{with \code{type="terms"} by default all terms are returned.
    A character vector specifies which terms are to be returned}
  \item{na.action}{function determining what should be done with missing
    values in \code{newdata}.  The default is to predict \code{NA}.}
  \item{\dots}{further arguments passed to or from other methods.}
}
\description{
  Obtains predictions and optionally estimates standard errors of those
  predictions from a fitted generalized linear model object.
}
\details{
  If \code{newdata} is omitted the predictions are based on the data
  used for the fit.  In that case how cases with missing values in the
  original fit is determined by the \code{na.action} argument of that
  fit.  If \code{na.action = na.omit} omitted cases will not appear in
  the residuals, whereas if \code{na.action = na.exclude} they will
  appear (in predictions and standard errors), with residual value
  \code{NA}.  See also \code{\link{napredict}}.
}
\value{
  If \code{se = FALSE}, a vector or matrix of predictions.  If \code{se
    = TRUE}, a list with components
  \item{fit}{Predictions}
  \item{se.fit}{Estimated standard errors}
  \item{residual.scale}{A scalar giving the square root of the
    dispersion used in computing the standard errors.}
}
\note{
  Variables are first looked for in \code{newdata} and then searched for
  in the usual way (which will include the environment of the formula
  used in the fit).  A warning will be given if the
  variables found are not of the same length as those in \code{newdata}
  if it was supplied.
}
\seealso{
  \code{\link{glm}}, \code{\link{SafePrediction}}
}

\examples{
require(graphics)

## example from Venables and Ripley (2002, pp. 190-2.)
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive=20-numdead)
budworm.lg <- glm(SF ~ sex*ldose, family=binomial)
summary(budworm.lg)

plot(c(1,32), c(0,1), type = "n", xlab = "dose",
     ylab = "prob", log = "x")
text(2^ldose, numdead/20, as.character(sex))
ld <- seq(0, 5, 0.1)
lines(2^ld, predict(budworm.lg, data.frame(ldose=ld,
   sex=factor(rep("M", length(ld)), levels=levels(sex))),
   type = "response"))
lines(2^ld, predict(budworm.lg, data.frame(ldose=ld,
   sex=factor(rep("F", length(ld)), levels=levels(sex))),
   type = "response"))
}
\keyword{models}
\keyword{regression}
