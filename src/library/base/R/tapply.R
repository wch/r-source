tapply <- function (X, INDEX, FUN=NULL, ..., simplify=TRUE)
{
    FUN <- if (!is.null(FUN)) match.fun(FUN)
    if (!is.list(INDEX)) INDEX <- list(INDEX)
    nI <- length(INDEX)
    namelist <- vector("list", nI)
    names(namelist) <- names(INDEX)
    extent <- integer(nI)
    nx <- length(X)
    one <- as.integer(1)
    group <- rep.int(one, nx)#- to contain the splitting vector
    ngroup <- one
    for (i in seq.int(INDEX)) {
	index <- as.factor(INDEX[[i]])
	if (length(index) != nx)
	    stop("arguments must have same length")
	namelist[[i]] <- levels(index)#- all of them, yes !
	extent[i] <- nlevels(index)
	group <- group + ngroup * (as.integer(index) - one)
	ngroup <- ngroup * nlevels(index)
    }
    if (is.null(FUN)) return(group)
    ans <- lapply(split(X, group), FUN, ...)
    index <- as.numeric(names(ans))
    if (simplify && all(unlist(lapply(ans, length)) == 1)) {
	ansmat <- array(dim=extent, dimnames=namelist)
	ans <- unlist(ans, recursive = FALSE)
    }
    else  {
	ansmat <- array(vector("list", prod(extent)),
			dim=extent, dimnames=namelist)
    }
    ## old : ansmat[as.numeric(names(ans))] <- ans
    names(ans) <- NULL
    ansmat[index] <- ans
    ansmat
}





