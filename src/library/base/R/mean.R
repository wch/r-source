mean <- function(x, ...) UseMethod("mean")

mean.default <- function(x, trim = 0, na.rm = FALSE) {
    if (na.rm)
	x <- x[!is.na(x)]
    trim <- trim[1]
    n <- length(c(x, recursive=TRUE)) # for data.frame
    if(trim > 0 && n > 0) {
	if(mode(x) == "complex")
	    stop("trimmed means are not defined for complex data")
	if(trim >= 0.5) return(median(x, na.rm=FALSE))
	lo <- floor(n*trim)+1
	hi <- n+1-lo
	x <- sort(x, partial=unique(c(lo, hi)))[lo:hi]
	n <- hi-lo+1
    }
    sum(x)/n
}

weighted.mean <- function(x, w, na.rm = FALSE ){
    if(missing(w)) w <- rep(1,length(x))
    if (na.rm) {
	w <- w[i <- !is.na(x)]
	x <- x[i]
    }
    sum(x*w)/sum(w)
}
