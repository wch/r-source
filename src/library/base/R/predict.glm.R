# file predict.glm.R
# copyright (C) 1998 W. N. Venables and B. D. Ripley
#
predict.glm <-
function(object, newdata=NULL, type = c("link", "response"), se.fit = F,
         dispersion = NULL, ...)
{
  predict.lm <- 
  function (object, newdata = model.frame(object), se=F) 
  {
    X <- model.matrix(delete.response(terms(object)), newdata)
    n <- NROW(object$qr$qr)
    p <- object$rank
    p1 <- 1:p
    piv <- object$qr$pivot[p1]
    r <- resid(object)
    f <- fitted(object)
    w <- weights(object)
    if (is.null(w)) rss <- sum(r^2)
    else rss <- sum(r^2 * w)
    R <- chol2inv(object$qr$qr[p1, p1, drop = FALSE])
    est <- object$coefficients[piv]
    predictor <- c(X[, piv, drop = F] %*% est)
    if(se) {
      ip <- real(NROW(X))
      for (i in (1:NROW(X))) {
        xi <- X[i, piv]
        ip[i] <- xi %*% R %*% xi
      }
      list(fit = predictor, se = sqrt(ip))
    } else predictor
  }
  
  type <- match.arg(type)
  if(!se.fit) {
    #No standard errors
    if(missing(newdata))
      pred <- switch(type,
                     link = object$linear.predictors,
                     response = object$fitted)
    else {
      pred <- predict.lm(object, newdata, se.fit)
      switch(type,
             response = {pred <- family(object)$linkinv(pred)},
             link = )
    }
  } else {
    pred <- predict.lm(object, newdata, se.fit)
    # summary.survreg has no ... argument.
    if(inherits(object, "survreg")) dispersion <- 1.
    if(is.null(dispersion) || dispersion == 0)
      dispersion <- summary(object, dispersion=dispersion)$dispersion
    fit <- pred$fit
    se.fit <- pred$se * sqrt(dispersion)
    residual.scale <- as.vector(sqrt(dispersion))
    switch(type,
	   response = {
	     fit <- family(object)$linkinv(fit)
	     se.fit <- pred$se * abs(family(object)$mu.eta(pred$fit))
	   }
	   ,
	   link = { }
	   )
    pred <- list(fit=fit, se.fit=se.fit, residual.scale=residual.scale)
  }
  pred
}
