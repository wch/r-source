\name{predict.glm}
\title{Predict Method for GLM Fits}
\usage{
predict.glm(object, newdata=NULL, type=c("link", "response"),
            se.fit=FALSE, dispersion=NULL, ...)
}
\alias{predict.glm}
\arguments{
 \item{object}{A fitted object of class inheriting from \code{"glm"}.}
 \item{newdata}{Optionally, a new data frame from which to make the
   predictions. If omitted, the fitted linear predictors are used.}
 \item{type}{The type of prediction required. The default is on the
   scale of the linear predictors; the alternative is on the scale of
   the response variable. Thus for a default binomial model the default
   predictions are of log-odds (probabilities on logit scale) and
   \code{type = "response"} gives the predicted probabilities.

   The value of this argument can be abbreviated.
 }
 \item{se.fit}{A switch indicating if standard errors are required.}
 \item{dispersion}{The dispersion of the GLM fit to be assumed in
   computing the standard errors. If omitted, that returned by
   \code{summary} applied to the object is used.}
}
\description{
 Obtains predictions and optionally estimates standard errors of those
 predictions from a fitted generalized linear model object.
}
\value{
  If \code{se = FALSE}, a vector of predictions. If \code{se = TRUE}, a
  list with components
  \item{fit}{Predictions}
  \item{se.fit}{Estimated standard errors}
  \item{residual.scale}{A scalar giving the square root of the
    dispersion used in computing the standard errors.}
}
\author{B.D. Ripley}
\note{This method is also currently used for objects of
  class \code{"survreg"} (parametric survival fits from package
  \code{survival4}) and possibly others. The assumptions made by
  \code{predict.glm} may not always be right for such objects.}

\seealso{\code{\link{glm}}}

\examples{
## example from Venables and Ripley (1997, pp. 231-3.)
ldose <- rep(0:5, 2)
numdead <- c(1, 4, 9, 13, 18, 20, 0, 2, 6, 10, 12, 16)
sex <- factor(rep(c("M", "F"), c(6, 6)))
SF <- cbind(numdead, numalive=20-numdead)
budworm.lg <- glm(SF ~ sex*ldose, family=binomial)
summary(budworm.lg)

plot(c(1,32), c(0,1), type="n", xlab="dose",
   ylab="prob", log="x")
text(2^ldose, numdead/20,as.character(sex))
ld <- seq(0, 5, 0.1)
lines(2^ld, predict(budworm.lg, data.frame(ldose=ld,
   sex=factor(rep("M", length(ld)), levels=levels(sex))),
   type="response"))
lines(2^ld, predict(budworm.lg, data.frame(ldose=ld,
   sex=factor(rep("F", length(ld)), levels=levels(sex))),
   type="response"))
}
\keyword{models}
\keyword{regression}
