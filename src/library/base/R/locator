locator <- function(n=1) {
	z <- .Internal(locator(n))
	x <- z[[1]]
	y <- z[[2]]
	n <- z[[3]]
	if(n==0) NULL else list(x=x[1:n],y=y[1:n])
}
