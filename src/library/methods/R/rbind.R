#  File src/library/methods/R/rbind.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2015 The R Core Team
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

#### S4-ized  rbind() --- this is entirely parallel to ./cbind() --- KEEP IN SYNC!
###  -------------------- built by
## s/cbind/rbind/ ; s/nrow/N_COL/; s/column/row/; s/colnam/rownam/;
## s/ncol/nrow/ ; s/N_COL/ncol/; s/d[2L]/d[1L]/

rbind <- function(..., deparse.level = 1)
{
    has.dl <- !missing(deparse.level)
    deparse.level <- as.integer(deparse.level)
    if(identical(deparse.level, -1L)) deparse.level <- 0L # our hack
    stopifnot(0 <= deparse.level, deparse.level <= 2)

    argl <- list(...)
    ## remove trailing 'NULL's:
    na <- nargs() - has.dl
    while(na > 0L && is.null(argl[[na]])) { argl <- argl[-na]; na <- na - 1L }
    if(na == 0) return(NULL)
    symarg <- as.list(substitute(list(...)))[-1L] # symbolic argument (names)
    nmsym <- names(symarg)
    ## Give *names* depending on deparse.level {for non-matrix}:
    nm <- c( ## 0:
	function(i) NULL,
	## 1:
	function(i) if(is.symbol(s <- symarg[[i]])) deparse(s) else NULL,
	## 2:
	function(i) deparse(symarg[[i]])[[1L]])[[ 1L + deparse.level ]]
    Nms <- function(i) { if(!is.null(s <- nmsym[i]) && nzchar(s)) s else nm(i) }
    if(na == 1) {
	if(isS4(..1)) {
	    r <- rbind2(..1)
	    if(length(dim(..1)) < 2L && length(dim(r)) == 2L)
		rownames(r) <- Nms(1)
	    return(r)
	}
	else return(.__H__.rbind(..., deparse.level = deparse.level))
    }

    ## else :  na >= 2

    if(na == 2) {
	r <- ..2
	fix.na <- FALSE
    }
    else { ## na >= 3 arguments: -- RECURSION -- with care
	## determine ncol(<result>)  for e.g.,	rbind(diag(2), 1, 2)
	## only when the last two argument have *no* dim attribute:
	nrs <- unname(lapply(argl, ncol)) # of length na
	iV <- vapply(nrs, is.null, NA)# is 'vector'
	fix.na <- identical(nrs[(na-1L):na], list(NULL,NULL))
	if(fix.na) {
	    ## "fix" last argument, using 1-row `matrix' of proper ncol():
	    nr <- max(if(all(iV)) lengths(argl) else unlist(nrs[!iV]))
	    argl[[na]] <- rbind(rep(argl[[na]], length.out = nr),
				deparse.level = 0)
	    ## and since it's a 'matrix' now, rbind() below may not name it
	}
	## need to pass argl, the evaluated arg list to do.call();
	## OTOH, these may have lost their original 'symbols'
	## if(deparse.level) {
	    if(fix.na)
		fix.na <- !is.null(Nna <- Nms(na))
	    if(!is.null(nmi <- names(argl))) iV <- iV & (nmi == "")
	    ## attach `symbols' to argl[-1L] for 'vectors'[iV]
	    ii <- if(fix.na) # need to fix later ([na] is 'matrix')
		2:(na-1) else 2:na
	    if(any(iV[ii])) {
		for(i in ii[iV[ii]])
		    if (!is.null(nmi <- Nms(i))) names(argl)[i] <- nmi
	    }
	## }
	r <- do.call(rbind, c(argl[-1L], list(deparse.level=deparse.level)))
    }

    d2 <- dim(r)
    r <- rbind2(..1, r) ## FIXME: add colnames depending on deparse.level
    if(deparse.level == 0)
	return(r)
    ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2L
    ism2 <- !is.null(d2)	     && length(d2) == 2L && !fix.na
    if(ism1 && ism2) ## two matrices
	return(r)

    ## else -- Setting rownames correctly
    ##	       when one was not a matrix [needs some diligence!]
    Nrow <- function(x) {
	d <- dim(x); if(length(d) == 2L) d[1L] else as.integer(length(x) > 0L) }
    nn1 <- !is.null(N1 <- if((l1 <- Nrow(..1)) && !ism1) Nms(1)) # else NULL
    nn2 <- !is.null(N2 <- if(na == 2 && Nrow(..2) && !ism2) Nms(2))
    if(nn1 || nn2 || fix.na) {
	if(is.null(rownames(r)))
	    rownames(r) <- rep.int("", nrow(r))
	setN <- function(i, nams)
	    rownames(r)[i] <<- if(is.null(nams)) "" else nams
	if(nn1) setN(1,	 N1)
	if(nn2) setN(1+l1, N2)
	if(fix.na) setN(nrow(r), Nna)
    }
    r
}
