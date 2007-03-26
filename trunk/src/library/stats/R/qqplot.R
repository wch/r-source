qqplot <- function(x, y, plot.it = TRUE, xlab = deparse(substitute(x)),
		   ylab = deparse(substitute(y)), ...)
{
    sx<-sort(x)
    sy<-sort(y)
    lenx<-length(sx)
    leny<-length(sy)
    if( leny < lenx )
	sx<-approx(1:lenx, sx, n=leny)$y
    if( leny > lenx )
	sy<-approx(1:leny, sy, n=lenx)$y
    if(plot.it)
	plot(sx, sy, xlab = xlab, ylab = ylab, ...)
    invisible(list(x = sx, y = sy))
}
