tapply <- function (x, INDEX, FUN=NULL, simplify=TRUE, ...) 
{
	if (is.character(FUN)) 
		FUN <- get(FUN, mode = "function")
	if (!is.null(FUN) && mode(FUN) != "function") 
		stop(paste("'", FUN, "' is not a function",sep=""))
	if (!is.list(INDEX)) INDEX <- list(INDEX)
	nI <- length(INDEX)
	namelist <- vector("list", nI)
	names(namelist) <- names(INDEX)
	extent <- integer(nI)
	nx <- length(x)
	group <- rep(1, nx)#- to contain the splitting vector
	ngroup <- 1
	for (i in seq(INDEX)) {
		index <- as.factor(INDEX[[i]])
		if (length(index) != nx) 
			stop("arguments must have same length")
		namelist[[i]] <- levels(index)#- all of them, yes !
		extent[i] <- nlevels(index)
		group <- group + ngroup * (as.numeric(index) - 1)
		ngroup <- ngroup * nlevels(index)
	}
	if (is.null(FUN)) return(group)
	ans <- lapply(split(x, group), FUN, ...)
	if (simplify && all(unlist(lapply(ans, length)) == 1)) {
		ansmat <- array(dim=extent, dimnames=namelist)
		ans <- unlist(ans, recursive = FALSE)
	}
	else  {
		ansmat <- array(vector("list", prod(extent)),
			dim=extent, dimnames=namelist)
	}
	# old : ansmat[as.numeric(names(ans))] <- ans
	index <- as.numeric(names(ans))
	names(ans) <- NULL
	ansmat[index] <- ans
	ansmat
}
