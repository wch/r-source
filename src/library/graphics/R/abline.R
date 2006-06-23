abline <- function(a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL,
                   coef = NULL, untf = FALSE, ...)
{
    int_abline <- function(a, b, h, v, untf, col = par("col"),
                           lty = par("lty"), lwd = par("lwd"), ...)
        .Internal(abline(a, b, h, v, untf, col, lty, lwd, ...))

    if(!is.null(reg)) a <- reg
    ## this should probably be is.object(a) || is.list(a)
    if(!is.null(a) && is.list(a)) {
	temp <- as.vector(coef(a))
	if(length(temp) == 1) {
	    a <- 0
	    b <- temp
	} else {
	    a <- temp[1]
	    b <- temp[2]
	}
    }
    if(!is.null(coef)) {
	a <- coef[1]
	b <- coef[2]
    }
    int_abline(a=a, b=b, h=h, v=v, untf=untf, ...)
    invisible()
}
