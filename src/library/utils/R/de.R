#  File src/library/utils/R/de.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

de.ncols <- function(inlist)
{
    ncols <- matrix(0, nrow=length(inlist), ncol=2L)
    i <- 1L
    for( telt in inlist ) {
	if( is.matrix(telt) ) {
	    ncols[i, 1L] <- ncol(telt)
	    ncols[i, 2L] <- 2L
	}
	else if( is.list(telt) ) {
	    for( telt2 in telt )
		if( !is.vector(telt2) ) stop("wrong argument to 'dataentry'")
	    ncols[i, 1L] <- length(telt)
	    ncols[i, 2L] <- 3L
	}
	else if( is.vector(telt) ) {
	    ncols[i, 1L] <- 1L
	    ncols[i, 2L] <- 1L
	}
	else stop("wrong argument to 'dataentry'")
	i <- i+1L
    }
    return(ncols)
}

de.setup <- function(ilist, list.names, incols)
{
    ilen <- sum(incols)
    ivec <- vector("list", ilen)
    inames <- vector("list", ilen)
    i <- 1L
    k <- 0L
    for( telt in ilist ) {
	k <- k+1L
	if( is.list(telt) ) {
	    y <- names(telt)
	    for( j in seq_along(telt) ) {
		ivec[[i]] <- telt[[j]]
		if( is.null(y) || y[j]=="" )
		    inames[[i]] <- paste0("var", i)
		else inames[[i]] <- y[j]
		i <- i+1L
	    }
	}
	else if( is.vector(telt) ) {
	    ivec[[i]] <- telt
	    inames[[i]] <- list.names[[k]]
	    i <- i+1
	}
	else if( is.matrix(telt) ) {
	    y <- dimnames(telt)[[2L]]
	    for( j in 1L:ncol(telt) ) {
		ivec[[i]] <- telt[, j]
		if( is.null(y) || y[j]=="" )
		    inames[[i]] <- paste0("var", i)
		else inames[[i]] <- y[j]
		i <- i+1L
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
    j <- 1L
    lnames <- names(inlist)
    if(p) for(i in 1L:p) {
	if(coltypes[i]==2) {
	    tlen <- length(inlist[[j]])
	    x <- matrix(0, nrow=tlen, ncol=ncols[i])
	    cnames <- vector("character", ncol(x))
	    for( ind1 in 1L:ncols[i]) {
		if(tlen != length(inlist[[j]]) ) {
		    warning("could not restore type information")
		    return(inlist)
		}
		x[, ind1] <- inlist[[j]]
		cnames[ind1] <- lnames[j]
		j <- j+1L
	    }
	    if( nrow(x) == nrow(args[[i]]) )
		rn <- dimnames(args[[i]])[[1L]]
	    else rn <- NULL
	    if( any(cnames!="") )
		dimnames(x) <- list(rn, cnames)
	    rlist[[i]] <- x
	    rnames[i] <- argnames[i]
	}
	else if(coltypes[i]==3) {
	    x <- vector("list", length=ncols[i])
	    cnames <- vector("character", ncols[i])
	    for( ind1 in 1L:ncols[i]) {
		x[[ind1]] <- inlist[[j]]
		cnames[ind1] <- lnames[j]
		j <- j+1L
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
    snames <- as.character(substitute(list(...))[-1L])
    if( is.null(sdata) ) {
	if( is.null(Names) ) {
	    odata <- vector("list", length=max(1,length(Modes)))
	}
	else {
	    if( (length(Names) != length(Modes)) && length(Modes) ) {
		warning("'modes' argument ignored")
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
	coltypes <- ncols[, 2L]
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

    if(any(coltypes != 1L)) {
	if(length(rdata) == sum(ncols))
	    rdata <- de.restore(rdata, ncols, coltypes, snames, sdata)
	else warning("could not restore variables properly")
    }
    return(rdata)
}

data.entry <- function(..., Modes=NULL, Names=NULL)
{
    tmp1 <- de(..., Modes=Modes, Names=Names)
    j <- 1L
    nn <- names(tmp1)
    for(i in nn) {
	assign(i, tmp1[[j]], envir=.GlobalEnv)
	j <- j+1L
    }
    if(j == 1L) warning("did not assign() anything")
    invisible(nn)
}
