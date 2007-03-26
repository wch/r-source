fivenum <- function(x, na.rm=TRUE)
{
    xna <- is.na(x)
    if(na.rm) x <- x[!xna]
    else if(any(xna)) return(rep.int(NA,5))
    x <- sort(x)
    n <- length(x)
    if(n == 0) rep.int(NA,5)
    else {
        n4 <- floor((n+3)/2) / 2
	d <- c(1, n4, (n+1)/2, n + 1 - n4, n)
	0.5*(x[floor(d)] + x[ceiling(d)])
    }
}
