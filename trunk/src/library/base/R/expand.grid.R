expand.grid <- function(...)
{
    ## x should either be a list or a set of vectors or factors
    nargs <- length(args <- list(...))
    if(! nargs) return(as.data.frame(list()))
    if(nargs == 1 && is.list(a1 <- args[[1]]))
        nargs <- length(args <- a1)
    if(nargs == 0) return(as.data.frame(list()))
    cargs <- args
    nmc <- paste("Var", 1:nargs, sep="")
    nm <- names(args)
    if(is.null(nm)) nm <- nmc
    if(any(ng0 <- nchar(nm) > 0)) nmc[ng0] <- nm[ng0]
    names(cargs) <- nmc
    rep.fac <- 1
    d <- sapply(args, length)
    dn <- vector("list", nargs)
    names(dn) <- nmc
    orep <- prod(d)
    for(i in 1:nargs) {
	x <- args[[i]]
        dn[[i]] <- paste(nmc[i], "=", if(is.numeric(x)) format(x) else x,
                         sep = "")
	nx <- length(x)
	orep <- orep/nx
	x <- x[rep.int(rep.int(seq(length = nx),
                               rep.int(rep.fac, nx)), orep)]
	## avoid sorting the levels of character variates
	if(!is.factor(x) && is.character(x)) x <- factor(x, levels = unique(x))
	cargs[[i]] <- x
	rep.fac <- rep.fac * nx
    }
    attr(cargs, "out.attrs") <- list(dim=d, dimnames=dn)
    structure(cargs, row.names = seq(length = prod(d)), colnames = nmc,
              class = "data.frame")
}
