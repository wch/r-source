# file predict.glm.R
# copyright (C) 1998 W. N. Venables and B. D. Ripley
#
predict.glm <-
function(object, newdata = NULL, type = c("link", "response"),
         se.fit = FALSE, dispersion = NULL, ...)
{
  ## 1998/06/23 KH:  predict.lm() now merged with the version in lm.R

  type <- match.arg(type)
  if(!se.fit) {
    ## No standard errors
    if(missing(newdata))
      pred <- switch(type,
                     link = object$linear.predictors,
                     response = object$fitted)
    else {
      pred <- predict.lm(object, newdata, se.fit, scale = 1)
      switch(type,
             response = {pred <- family(object)$linkinv(pred)},
             link = )
    }
  } else {
    ## summary.survreg has no ... argument.
    if(inherits(object, "survreg")) dispersion <- 1.
    if(is.null(dispersion) || dispersion == 0)
      dispersion <- summary(object, dispersion=dispersion)$dispersion
    residual.scale <- as.vector(sqrt(dispersion))
    pred <- predict.lm(object, newdata, se.fit, scale = residual.scale)
    fit <- pred$fit
    se.fit <- pred$se.fit
    switch(type,
	   response = {
	     fit <- family(object)$linkinv(fit)
	     se.fit <- se.fit * abs(family(object)$mu.eta(fit))
	   },
	   link = )
    pred <- list(fit=fit, se.fit=se.fit, residual.scale=residual.scale)
  }
  pred
}
