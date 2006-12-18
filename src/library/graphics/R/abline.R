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
        ## was  (!is.null(a) && is.list(a))
	coefa <- as.vector(coef(a))
	if (length(coefa) > 2)
	    warning("only using the first two of ", length(coefa),
		    "regression coefficients")
	has.int <- !is.null(ii <- attr(terms(a), "intercept")) && as.logical(ii)
	if (!has.int) {
	    a <- 0
	    b <- coefa[1]
	} else {
	    a <- coefa[1]
	    b <- if (length(coefa) >= 2) coefa[2] else 0
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
