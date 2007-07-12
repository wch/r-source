mean <- function(x, ...) UseMethod("mean")

mean.default <- function(x, trim = 0, na.rm = FALSE, ...)
{
    if(!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(NA_real_)
    }
    if (na.rm)
	x <- x[!is.na(x)]
    if(!is.numeric(trim) || length(trim) != 1)
        stop("'trim' must be numeric of length one")
    n <- length(x)
    if(trim > 0 && n > 0) {
	if(is.complex(x))
	    stop("trimmed means are not defined for complex data")
	if(trim >= 0.5) return(stats::median(x, na.rm=FALSE))
	lo <- floor(n*trim)+1
	hi <- n+1-lo
	x <- sort.int(x, partial=unique(c(lo, hi)))[lo:hi]
    }
    .Internal(mean(x))
}

mean.data.frame <- function(x, ...) sapply(x, mean, ...)
