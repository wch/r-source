sort <- function(x, partial=NULL, na.last=NA) {
    isfact<-is.factor(x)
    if(isfact){
	lev<-levels(x)
	nlev<-nlevels(x)
    }
    nas <- x[is.na(x)]
    x <- c(x[!is.na(x)])
    if(!is.null(partial))
	y <- .Internal(psort(x, partial))
    else {
	nms <- names(x)
	if(!is.null(nms)) {
	    o <- order(x)
	    y <- x[o]
	    names(y) <- nms[o]
	}
	else
	    y <- .Internal(sort(x))
    }
    if(!is.na(na.last)) {
	if(!na.last) y <- c(nas, y)
	else if (na.last) y <- c(y, nas)
    }
    if(isfact) y<-factor(y,levels=1:nlev,labels=lev)
    y
}
