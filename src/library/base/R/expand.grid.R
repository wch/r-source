## file expand.grid.R
## copyright (C) 1998 W. N. Venables and B. D. Ripley
##
expand.grid <- function(...) {
    ## x should either be a list or a set of vectors or factors
    args <- list(...)
    if(!length(args)) return(NULL)
    a1 <- args[[1]]
    if(length(args) == 1 && is.list(a1)) args <- a1
    nargs <- length(args)
    if(nargs == 1) return (args[[1]])
    cargs <- args
    nmc <- paste("Var", 1:nargs, sep="")
    nm <- names(args)
    if(is.null(nm)) nm <- nmc
    nmc[nchar(nm)>0] <- nm[nchar(nm)>0]
    names(cargs) <- nmc
    rep.fac <- 1
    orep <- final.len <- prod(sapply(args, length))
    for(i in 1:nargs) {
	x <- args[[i]]
	## avoid sorting the levels of character variates
	nx <- length(x)
	orep <- orep/nx
	x <- rep(rep(x, rep(rep.fac, nx)), orep)
	## avoid sorting the levels of character variates
	if(!is.factor(x) && is.character(x)) x <- factor(x, levels = unique(x))
	cargs[[i]] <- x
	rep.fac <- rep.fac * nx
    }
    do.call("cbind.data.frame", cargs)
}
