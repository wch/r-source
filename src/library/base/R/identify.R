identify <- function(x, ...) UseMethod("identify")

identify.default <- function(x, y=NULL, labels=seq(along=x), pos=FALSE,
			     n=length(x), plot=TRUE, offset=0.5, ...)
{
    if(length(extras <- list(...))) {
        opar <- par(extras)
        on.exit(par(opar))
    }
    xy <- xy.coords(x, y)
    x <- xy$x
    y <- xy$y
    if (length(x)==0){
        if (pos)
            return(list(ind=numeric(0), pos=numeric(0)))
        else
            return(numeric(0))
    }
    z <- .Internal(identify(x, y, as.character(labels), n, plot, offset))
    i <- seq(z[[1]])[z[[1]]]
    if(pos) list(ind = i, pos = z[[2]][z[[1]]]) else i
}
