confint <- function(object, parm, level = 0.95, ...) UseMethod("confint")

format.perc <- function(probs, digits)
    ## Not yet exported, maybe useful in other contexts:
    ## quantile.default() sometimes uses a version of it
    paste(format(100 * probs, trim = TRUE, scientific = FALSE, digits = digits),
	  "%")

confint.lm <- function(object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    pnames <- names(cf)
    if(missing(parm))
	parm <- seq_along(pnames)
    else if(is.character(parm))
	parm <- match(parm, pnames, nomatch = 0)
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- format.perc(a, 3)
    ci <- array(NA, dim = c(length(parm), 2),
		dimnames = list(pnames[parm], pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    fac <- qt(a, object$df.residual)
    ci[] <- cf[parm] + ses %o% fac
    ci
}

confint.glm <- function(object, parm, level = 0.95, ...)
    try(MASS:::confint.glm(object, parm, level, ...))

confint.nls <- function(object, parm, level = 0.95, ...)
    try(MASS:::confint.nls(object, parm, level, ...))

confint.default <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm))
	parm <- seq_along(pnames)
    else if (is.character(parm))
	parm <- match(parm, pnames, nomatch = 0)
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2),
		dimnames = list(pnames[parm], pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}
