## mlm := multivariate lm()
summary.mlm <- function(object, ...)
{
    coef <- coef(object)
    ny <- ncol(coef)
## Not right:    if(is.null(ny)) return(NextMethod("summary"))
    effects <- object$effects
    resid <- residuals(object)
    fitted <- fitted(object)
    ynames <- colnames(coef)
    if(is.null(ynames)) {
	lhs <- object$terms[[2]]
	if(mode(lhs) == "call" && lhs[[1]] == "cbind")
	    ynames <- as.character(lhs)[-1]
	else ynames <- paste("Y", seq(ny), sep = "")
    }
    value <- vector("list", ny)
    names(value) <- paste("Response", ynames)
    cl <- oldClass(object)
    class(object) <- cl[match("mlm", cl):length(cl)][-1]
    for(i in seq(ny)) {
	object$coefficients <- coef[, i]
	object$residuals <- resid[, i]
	object$fitted.values <- fitted[, i]
	object$effects <- effects[, i]
	object$call$formula[[2]] <- object$terms[[2]] <- as.name(ynames[i])
	value[[i]] <- summary(object, ...)
    }
    class(value) <- "listof"
    value
}
## predict.mlm  is in  >> ./lm.R <<
anova.mlm <- function(object, ...)
    stop("no anova method implemented for mlm models")

deviance.mlm <- function(object, ...)
{
    res <-
	if(is.null(w <- object$weights)) object$residuals^2
	else w * object$residuals^2
    drop(rep.int(1, nrow(res)) %*% res)
}

plot.mlm <- function (x, ...) .NotYetImplemented()
