mean <- function(x, ...) UseMethod("mean")

mean.default <- function(x, trim = 0, na.rm = FALSE) {
        if (na.rm)
                x<-x[!is.na(x)]
        trim <- trim[1]
	if(trim > 0) {
		if(trim >= 0.5) return(median(x, na.rm=FALSE))
		lo <- floor(length(x)*trim)+1
		hi <- length(x)+1-lo
		x <- sort(x, partial=unique(c(lo, hi)))[lo:hi]
	}
        sum(x)/length(x)
}

weighted.mean <- function(x, w, na.rm = FALSE ){
	if(missing(w)) w <- rep(1,length(x))
	if (na.rm) {
		w<-w[!is.na(x)]
		x<-x[!is.na(x)]
	}
	sum(x*w)/sum(w)
}
