###- FIXME --- This is UGLY :  a lot of coding is just doubled from  ./glm.R --

anova.glm.null <- function (object, ..., test = NULL, na.action = na.omit)
{
    ## check for multiple objects
    if (length(list(object, ...)) > 1)
	return(anova.glmlist(list(object, ...), test = test))
    ## extract variables from model
    varlist <- attr(object$terms, "variables")
    nvars <- 0
    resdev <- resdf <- NULL
    ## if there is more than one explanatory variable then
    ## recall glm.fit to fit variables sequentially
    ## add values from null and full model
    resdf <- c(object$df.null)
    resdev <- c(object$null.deviance)
    ## construct table and title
    table <- data.frame(c(NA), c(NA), resdf, resdev)
    dimnames(table) <- list(c("NULL", attr(object$terms, "term.labels")),
                            c("Df", "Deviance", "Resid. Df", "Resid. Dev"))
    title <- paste("Analysis of Deviance Table", "\n\nModel: ",
		   object$family$family, ", link: ", object$family$link,
		   "\n\nResponse: ", as.character(varlist[-1])[1],
		   "\n\nTerms added sequentially (first to last)\n\n",
		   sep = "")
    ## calculate test statistics if needed
    ## return output
    if (!is.null(test))
	table <- stat.anova(table = table, test = test,
			    scale = sum(object$weights * object$residuals^2)/
                            	object$df.residual,
			    df.scale = object$df.residual, n = NROW(x))
    output <- list(title = title, table = table)
    class(output) <- c("anova.glm.null", "anova.glm")
    return(output)
}
print.glm.null <- function(x, digits = max(3, getOption("digits") - 3),
                           na.print = "", ...)
{
    cat("\nCall: ", deparse(x$call), "\n\n")
    cat("No coefficients\n")
    cat("\nDegrees of Freedom:", length(x$residuals), "Total;",
	x$df.residual, "Residual\n")
    cat("Null Deviance:", format(signif(x$null.deviance, digits)), "\n")
    cat("Residual Deviance:", format(signif(x$deviance, digits)), "\t")
    cat("AIC:", format(signif(x$aic, digits)), "\n")
    invisible(x)
}
print.summary.glm.null <- function (x, digits = max(3, getOption("digits") - 3),
                                    na.print = "", ...)
{
    cat("\nCall:\n")
    cat(paste(deparse(x$call), sep = "\n", collapse = "\n"),
	"\n\n", sep = "")
    cat("Deviance Residuals: \n")
    if (x$df.residual > 5) {
	x$deviance.resid <- quantile(x$deviance.resid)
	names(x$deviance.resid) <- c("Min", "1Q", "Median",
				     "3Q", "Max")
    }
    print.default(x$deviance.resid, digits = digits, na = "", print.gap = 2)
    cat("\nNo coefficients\n")
    cat(paste("\n(Dispersion parameter for ", x$family$family,
	      " family taken to be ", x$dispersion, ")\n\n    Null deviance: ",
	      x$null.deviance, " on ", x$df.null, " degrees of freedom\n\n",
	      "Residual deviance: ", x$deviance, " on ", x$df.residual,
	      " degrees of freedom\n\n", "Number of Fisher Scoring iterations: ",
	      x$iter, "\n\n", sep = ""))
    invisible(x)
}
summary.glm.null <- function (object, dispersion = NULL, correlation = TRUE,
                              na.action = na.omit, ...)
{
    ## calculate dispersion if needed
    ## extract x to get column names
    ## calculate scaled and unscaled covariance matrix
    if (is.null(dispersion)) {
	if (any(object$family$family == c("poisson",
		"binomial")))
	    dispersion <- 1
	else {
	    if (any(object$weights == 0))
		warning(paste("observations with zero weight",
			      "not used for calculating dispersion"))
	    dispersion <- sum(object$weights * object$residuals^2)/
                object$df.residual
	}
    }
    p <- 0
    ## return answer
    ans <- list(call = object$call, terms = object$terms,
		family = object$family,
                deviance.resid = residuals(object, type = "deviance"),
                dispersion= dispersion, df = c(object$rank,object$df.residual),
                deviance = object$deviance, df.residual = object$df.residual,
                null.deviance = object$null.deviance,
		df.null = object$df.null, iter = object$iter,
		)
    class(ans) <- c("summary.glm.null", "summary.glm")
    return(ans)
}
glm.fit.null <-
    function (x, y, weights = rep(1, nobs), start = NULL,
              etastart = NULL, mustart = NULL, offset = rep(0, nobs),
              family = gaussian(), control = glm.control(), intercept = FALSE)
{
    if(intercept) stop("null models have no intercept")
    ynames <- names(y)
    conv <- TRUE
    nobs <- NROW(y)
    nvars <- NCOL(x)
    ## define weights and offset if needed
    ## get family functions
    if (is.null(weights))
	weights <- rep(1, nobs)
    if (is.null(offset))
	offset <- rep(0, nobs)
    variance <- family$variance
    dev.resids <- family$dev.resids
    linkinv <- family$linkinv
    mu.eta <- family$mu.eta
    valideta <- family$valideta
    if (is.null(valideta))
	valideta <- function(eta) TRUE
    validmu <- family$validmu
    if (is.null(validmu))
	validmu <- function(mu) TRUE
	## next line may change y and weights, and set n.
    eval(family$initialize)
    if (NCOL(y) > 1)
	stop("y must be univariate unless binomial")
    eta <- rep(0, nobs)
    if (!valideta(eta + offset))
	stop("Invalid linear predictor values in empty model")
    mu <- linkinv(eta + offset)
    ## calculate initial deviance and coefficient
    if (!validmu(mu))
	stop("Invalid fitted means in empty model")
    dev <- sum(dev.resids(y, mu, weights))
    w <- ((weights * mu.eta(eta + offset)^2)/variance(mu))^0.5
    ##	residuals[good] <- z - eta
    residuals <- (y - mu)/mu.eta(eta + offset)
    ## name output
    names(residuals) <- ynames
    names(mu) <- ynames
    names(eta) <- ynames
    names(w) <- ynames
    names(weights) <- ynames
    names(y) <- ynames
    ## calculate null deviance
    wtdmu <- linkinv(offset)
    nulldev <- sum(dev.resids(y, wtdmu, weights))
    ## calculate df
    resdf <- nulldf <- n.ok <- nobs - sum(weights==0)
    aic.model <- family$aic(y, n, mu, weights, dev)
    return(list(coefficients = numeric(0), residuals = residuals,
		fitted.values = mu, rank = 0, family = family,
		linear.predictors = eta + offset, deviance = dev,
		aic = aic.model,
		null.deviance = nulldev, iter = 0, weights = w^2,
		prior.weights = weights, df.residual = resdf,
		df.null = nulldf, y = y, converged = conv, boundary = FALSE))
}

model.matrix.glm.null<-function(object, ...){
  rval<-matrix(ncol=0,nrow=length(object$y))
  attr(rval,"assign")<-integer(0)
}
