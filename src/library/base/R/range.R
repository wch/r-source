range <- function(..., na.rm=FALSE, finite=FALSE)
	if(finite) { x <- c(...); x <- x[is.finite(x)]; c(min(x), max(x))
	} else c(min(..., na.rm=na.rm),max(..., na.rm=na.rm))
