## file expand.grid.R
## copyright (C) 1998 W. N. Venables and B. D. Ripley
##
expand.grid <- function(...) {
    ## x should either be a list or a set of vectors or factors
    nargs <- length(args <- list(...))
    if(! nargs) return(as.data.frame(list()))
    if(nargs == 1 && is.list(a1 <- args[[1]]))
        nargs <- length(args <- a1)
    if(nargs <= 1)
        return(as.data.frame(if(nargs==0||is.null(args[[1]])) list() else args,
                             optional = TRUE))
    cargs <- args
    nmc <- paste("Var", 1:nargs, sep="")
    nm <- names(args)
    if(is.null(nm)) nm <- nmc
    if(any(ng0 <- nchar(nm) > 0)) nmc[ng0] <- nm[ng0]
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
