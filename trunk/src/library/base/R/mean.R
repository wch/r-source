mean <- function(x, ...) UseMethod("mean")

mean.default <- function(x, trim = 0, na.rm = FALSE, ...)
{
    if(!is.numeric(x) && !is.complex(x) && !is.logical(x)) {
        warning("argument is not numeric or logical: returning NA")
        return(as.numeric(NA))
    }
    if (na.rm)
	x <- x[!is.na(x)]
    trim <- trim[1]
    n <- length(x)
    if(trim > 0 && n > 0) {
	if(is.complex(x))
	    stop("trimmed means are not defined for complex data")
	if(trim >= 0.5) return(median(x, na.rm=FALSE))
	lo <- floor(n*trim)+1
	hi <- n+1-lo
	x <- sort(x, partial=unique(c(lo, hi)))[lo:hi]
	n <- hi-lo+1
    }
    .Internal(mean(x))
}

mean.data.frame <- function(x, ...) sapply(x, mean, ...)
