identify <- function(x, y=NULL, labels= seq(along=x), pos=FALSE, ...) {
	opar <- par(list(...))
	on.exit(par(opar))
	xy <- xy.coords(x, y)
	z <- .Internal(identify(xy$x,xy$y,as.character(labels)))
	i <- seq(z[[1]])[z[[1]]]
	p <- z[[2]][z[[1]]]
	if(pos) list(ind=i,pos=p) else i
}
