abline <-
    function(a=NULL, b=NULL, h=NULL, v=NULL, reg=NULL, coef=NULL,
	     untf=FALSE, col=par("col"), lty=par("lty"), lwd=par("lwd"), ...)
{
    if(!is.null(reg)) a <- reg
    if(!is.null(a) && is.list(a)) {
	temp <- as.vector(coefficients(a))
	if(length(temp) == 1) {
	    a <- 0
	    b <- temp
	}
	else {
	    a <- temp[1]
	    b <- temp[2]
	}
    }
    if(!is.null(coef)) {
	a <- coef[1]
	b <- coef[2]
    }
    .Internal(abline(a, b, h, v, untf, col, lty, lwd, ...))
    invisible()
}
