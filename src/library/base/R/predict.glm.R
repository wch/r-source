predict.glm <-
  function(object, newdata = NULL, type = c("link", "response", "terms"),
           se.fit = FALSE, dispersion = NULL, terms=NULL,
           na.action = na.pass, ...)
{
    ## 1998/06/23 KH:  predict.lm() now merged with the version in lm.R

    type <- match.arg(type)
    na.act <- object$na.action
    object$na.action <- NULL # kill this for predict.lm calls
    if (!se.fit) {
	## No standard errors
	if(missing(newdata)) {
	    pred <- switch(type,
			   link = object$linear.predictors,
			   response = object$fitted,
                           terms = predict.lm(object,  se.fit=se.fit,
                               scale = 1, type="terms", terms=terms)
                           )
            if(!is.null(na.act)) pred <- napredict(na.act, pred)
	} else {
	    pred <- predict.lm(object, newdata, se.fit, scale = 1,
                               type = ifelse(type=="link", "response", type),
                               terms = terms, na.action = na.action)
	    switch(type,
		   response = {pred <- family(object)$linkinv(pred)},
		   link =, terms= )
          }
    } else {
	## summary.survreg has no ... argument.
	if(inherits(object, "survreg")) dispersion <- 1.
	if(is.null(dispersion) || dispersion == 0)
	    dispersion <- summary(object, dispersion=dispersion)$dispersion
	residual.scale <- as.vector(sqrt(dispersion))
	pred <- predict.lm(object, newdata, se.fit, scale = residual.scale,
                           type = ifelse(type=="link", "response", type),
                           terms = terms, na.action = na.action)
	fit <- pred$fit
	se.fit <- pred$se.fit
	switch(type,
	       response = {
		   se.fit <- se.fit * abs(family(object)$mu.eta(fit))
		   fit <- family(object)$linkinv(fit)
	       },
	       link =, terms=)
        if( missing(newdata) && !is.null(na.act) ) {
            fit <- napredict(na.act, fit)
            se.fit <- napredict(na.act, se.fit)
        }
	pred <- list(fit=fit, se.fit=se.fit, residual.scale=residual.scale)
    }
    pred
}
