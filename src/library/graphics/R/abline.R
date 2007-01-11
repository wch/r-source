abline <- function(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL,
                   coef = NULL, untf = FALSE, ...)
{
    int_abline <- function(a, b, h, v, untf, col = par("col"),
                           lty = par("lty"), lwd = par("lwd"), ...)
        .Internal(abline(a, b, h, v, untf, col, lty, lwd, ...))

    if(!is.null(reg)) {
        if(!is.null(a)) warning("'a' is overridden by 'reg'")
        a <- reg
    }
    if(is.object(a) || is.list(a)) {
	## was	(!is.null(a) && is.list(a))
	p <- length(coefa <- as.vector(coef(a)))
	if (p > 2)
	    warning("only using the first two of ",p, "regression coefficients")
	islm <- inherits(a, "lm")
	noInt <- if(islm) !as.logical(attr(stats::terms(a), "intercept")) else p == 1
	if (noInt) {
	    a <- 0
	    b <- coefa[1]
	} else {
	    a <- coefa[1]
	    b <- if (p >= 2) coefa[2] else 0
	}
    }
    if(!is.null(coef)) {
	if(!is.null(a)) warning("'a' and 'b' are overridden by 'coef'")
	a <- coef[1]
	b <- coef[2]
    }
    int_abline(a=a, b=b, h=h, v=v, untf=untf, ...)
    invisible()
}
