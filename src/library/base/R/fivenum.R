fivenum <- function(x, na.rm=TRUE)
{
    xna <- is.na(x)
    if(na.rm) x <- x[!xna]
    else if(any(xna)) return(rep.int(NA,5))
    x <- sort(x)
    n <- length(x)
    if(n == 0) rep.int(NA,5)
    else {
	d <- c(1, 0.5*floor(0.5*(n+3)), 0.5*(n+1),
	       n+1-0.5*floor(0.5*(n+3)), n)
	0.5*(x[floor(d)]+x[ceiling(d)])
    }
}
