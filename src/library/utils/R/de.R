de.ncols <- function(inlist)
{
    ncols <- matrix(0, nrow=length(inlist), ncol=2)
    i <- 1
    for( telt in inlist ) {
	if( is.matrix(telt) ) {
	    ncols[i, 1] <- ncol(telt)
	    ncols[i, 2] <- 2
	}
	else if( is.list(telt) ) {
	    for( telt2 in telt )
		if( !is.vector(telt2) ) stop("wrong argument to 'dataentry'")
	    ncols[i, 1] <- length(telt)
	    ncols[i, 2] <- 3
	}
	else if( is.vector(telt) ) {
	    ncols[i, 1] <- 1
	    ncols[i, 2] <- 1
	}
	else stop("wrong argument to 'dataentry'")
	i <- i+1
    }
    return(ncols)
}

de.setup <- function(ilist, list.names, incols)
{
    ilen <- sum(incols)
    ivec <- vector("list", ilen)
    inames <- vector("list", ilen)
    i <- 1
    k <- 0
    for( telt in ilist ) {
	k <- k+1
	if( is.list(telt) ) {
	    y <- names(telt)
	    for( j in 1:length(telt) ) {
		ivec[[i]] <- telt[[j]]
		if( is.null(y) || y[j]=="" )
		    inames[[i]] <- paste("var", i, sep="")
		else inames[[i]] <- y[j]
		i <- i+1
	    }
	}
	else if( is.vector(telt) ) {
	    ivec[[i]] <- telt
	    inames[[i]] <- list.names[[k]]
	    i <- i+1
	}
	else if( is.matrix(telt) ) {
	    y <- dimnames(telt)[[2]]
	    for( j in 1:ncol(telt) ) {
		ivec[[i]] <- telt[, j]
		if( is.null(y) || y[j]=="" )
		    inames[[i]] <- paste("var", i, sep="")
		else inames[[i]] <- y[j]
		i <- i+1
	    }
	}
	else stop("wrong argument to 'dataentry'")
    }
    names(ivec) <- inames
    return(ivec)
}

de.restore <- function(inlist, ncols, coltypes, argnames, args)
{
    ## take the data in inlist and restore it
    ## to the format described by ncols and coltypes
    p <- length(ncols)
    rlist <- vector("list", length=p)
    rnames <- vector("character", length=p)
    j <- 1
    lnames <- names(inlist)
    if(p) for(i in 1:p) {
	if(coltypes[i]==2) {
	    tlen <- length(inlist[[j]])
	    x <- matrix(0, nrow=tlen, ncol=ncols[i])
	    cnames <- vector("character", ncol(x))
	    for( ind1 in 1:ncols[i]) {
		if(tlen != length(inlist[[j]]) ) {
		    warning("could not restore type information")
		    return(inlist)
		}
		x[, ind1] <- inlist[[j]]
		cnames[ind1] <- lnames[j]
		j <- j+1
	    }
	    if( dim(x) == dim(args[[i]]) )
		rn <- dimnames(args[[i]])[[1]]
	    else rn <- NULL
	    if( any(cnames!="") )
		dimnames(x) <- list(rn, cnames)
	    rlist[[i]] <- x
	    rnames[i] <- argnames[i]
	}
	else if(coltypes[i]==3) {
	    x <- vector("list", length=ncols[i])
	    cnames <- vector("character", ncols[i])
	    for( ind1 in 1:ncols[i]) {
		x[[ind1]] <- inlist[[j]]
		cnames[ind1] <- lnames[j]
		j <- j+1
	    }
	    if( any(cnames!="") )
		names(x) <- cnames
	    rlist[[i]] <- x
	    rnames[i] <- argnames[i]
	}
	else {
	    rlist[[i]] <- inlist[[j]]
	    j <- j+1
	    rnames[i] <- argnames[i]
	}
    }
    names(rlist) <- rnames
    return(rlist)
}

de <- function(..., Modes=list(), Names=NULL)
{
    sdata <- list(...)
    snames <- as.character(substitute(list(...))[-1])
    if( is.null(sdata) ) {
	if( is.null(Names) ) {
	    odata <- vector("list", length=max(1,length(Modes)))
	}
	else {
	    if( (length(Names) != length(Modes)) && length(Modes) ) {
		warning("modes argument ignored")
		Modes <- list()
	    }
	    odata <- vector("list", length=length(Names))
	    names(odata) <- Names
	}
	ncols <- rep.int(1, length(odata))
	coltypes <- rep.int(1, length(odata))
    }
    else {
	ncols <- de.ncols(sdata)
	coltypes <- ncols[, 2]
	ncols <- ncols[, 1]
	odata <- de.setup(sdata, snames, ncols)
	if(length(Names))
	    if( length(Names) != length(odata) )
		warning("'names' argument ignored")
	    else names(odata) <- Names
	if(length(Modes))
	    if(length(Modes) != length(odata)) {
		warning("'modes' argument ignored")
		Modes <- list()
	    }
    }
    rdata <- dataentry(odata, as.list(Modes))

    if(any(coltypes != 1)) {
	if(length(rdata) == sum(ncols))
	    rdata <- de.restore(rdata, ncols, coltypes, snames, sdata)
	else warning("could not restore variables properly")
    }
    return(rdata)
}

data.entry <- function(..., Modes=NULL, Names=NULL)
{
    tmp1 <- de(..., Modes=Modes, Names=Names)
    j <- 1
    nn <- names(tmp1)
    for(i in nn) {
	assign(i, tmp1[[j]], env=.GlobalEnv)
	j <- j+1
    }
    if(j==1) warning("did not assign() anything")
    invisible(nn)
}
