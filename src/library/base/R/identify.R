identify <- function(x, ...) UseMethod("identify")

identify.default <- function(x, y=NULL, labels=seq(along=x), pos=FALSE,
			     n=length(x), plot=TRUE, offset=0.5, ...)
{
    opar <- par(list(...))
    on.exit(par(opar))
    xy <- xy.coords(x, y)
    z <- .Internal(identify(xy$x, xy$y, as.character(labels),
			    n, plot, offset))
    i <- seq(z[[1]])[z[[1]]]
    if(pos) list(ind= i, pos= z[[2]][z[[1]]]) else i
}
