sort <- function(x, partial=NULL, na.last=NA)
{
    isfact <- is.factor(x)
    if(isfact){
	lev <- levels(x)
	nlev <- nlevels(x)
    }
    nas <- x[is.na(x)]
    x <- c(x[!is.na(x)])
    if(!is.null(partial)) {
        if(!all(is.finite(partial))) stop("non-finite `partial'")
	y <- .Internal(psort(x, partial))
    } else {
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
    if(isfact) y <- factor(y, levels=1:nlev, labels=lev)
    y
}

order <- function(..., na.last = TRUE)
{
    if(!is.na(na.last))
        .Internal(order(na.last, ...))
    else{ ## remove nas
        z <- list(...)
        if(any(diff(sapply(z, length)) != 0))
            stop("Argument lengths differ")
        ok <- !apply(sapply(z, is.na), 1, any)
        if(all(!ok)) stop("all elements contain an NA")
        z[[1]][!ok] <- NA
        ans <- do.call("order", z)
        keep <- seq(along=ok)[ok]
        ans[ans %in% keep]
    }
}

sort.list <- function(x, partial = NULL, na.last = TRUE)
{
    if(!is.null(partial))
        .NotYetUsed("partial != NULL")
    .Internal(order(na.last, x))
}
