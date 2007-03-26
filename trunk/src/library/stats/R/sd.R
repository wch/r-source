sd <- function(x, na.rm=FALSE) {
    if (is.matrix(x))
	apply(x, 2, sd, na.rm=na.rm)
    else if (is.vector(x))
	sqrt(var(x, na.rm=na.rm))
    else if (is.data.frame(x))
	sapply(x, sd, na.rm=na.rm)
    else 
	sqrt(var(as.vector(x), na.rm=na.rm))
}
