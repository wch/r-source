#  File src/library/methods/R/cbind.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2014 The R Core Team
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

#### cbind() and rbind()  which build on  cbind2() / rbind2()
#### --------------------------------------------------------
### NOTE: We rely on
### o	dim(.) working reliably for all arguments of [cr]bind2()
### o	All [cr]bind2() methods are assumed to
###	       1) correctly set (row/col)names
###	       2) correctly set (col/row)names for *matrix*(like) arguments

### Note that this
### 1) is namespace-hidden usually,
### 2) cbind / rbind are almost never called in 'methods' itself
### hence, the following has almost no effect unless ``activated'' (see below)

## rbind() is in ./rbind.R  {so it's easier to keep them 100% - synchronized !}

cbind <- function(..., deparse.level = 1)
{
    na <- nargs() - !missing(deparse.level)
    deparse.level <- as.integer(deparse.level)
    stopifnot(0 <= deparse.level, deparse.level <= 2)

    argl <- list(...)
    ## remove trailing 'NULL's:
    while(na > 0 && is.null(argl[[na]])) { argl <- argl[-na]; na <- na - 1 }
    if(na == 0) return(NULL)
    if(na == 1) {
	if(isS4(..1)) return(cbind2(..1))
	else return(.__H__.cbind(..., deparse.level = deparse.level))
    }

    ## else :  na >= 2

    if(deparse.level) {
	symarg <- as.list(sys.call()[-1L])[1L:na] # the unevaluated arguments
	## For default 'deparse.level = 1', cbind(a, b) has to give *names*!
	Nms <- function(i) { # possibly 'deparsed' names of argument  i
	    if(is.null(r <- names(symarg[i])) || r == "") {
		if(is.symbol(r <- symarg[[i]]) || deparse.level == 2)
		    deparse(r)		# else NULL
	    } else r
	}
    }
    if(na == 2) {
	r <- ..2
	fix.na <- FALSE
    }
    else { ## na >= 3 arguments: -- RECURSION -- with care
	## determine nrow(<result>)  for e.g.,	cbind(diag(2), 1, 2)
	## only when the last two argument have *no* dim attribute:
	nrs <- unname(lapply(argl, nrow)) # of length na
	iV <- vapply(nrs, is.null, NA)# is 'vector'
	fix.na <- identical(nrs[(na-1):na], list(NULL,NULL))
	if(fix.na) {
	    ## "fix" last argument, using 1-column `matrix' of proper nrow():
	    nr <- max(if(all(iV)) sapply(argl, length) else unlist(nrs[!iV]))
	    argl[[na]] <- cbind(rep(argl[[na]], length.out = nr),
				deparse.level = 0)
	    ## and since it's a 'matrix' now, cbind() below may not name it
	}
	## need to pass argl, the evaluated arg list to do.call();
	## OTOH, these may have lost their original 'symbols'
	if(deparse.level) {
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
	}
	r <- do.call(cbind, c(argl[-1L], list(deparse.level=deparse.level)))
    }

    d2 <- dim(r)
    r <- cbind2(..1, r)
    if(deparse.level == 0)
	return(r)
    ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2L
    ism2 <- !is.null(d2)	     && length(d2) == 2L && !fix.na
    if(ism1 && ism2) ## two matrices
	return(r)

    ## else -- Setting colnames correctly
    ##	       when one was not a matrix [needs some diligence!]
    Ncol <- function(x) {
	d <- dim(x); if(length(d) == 2L) d[2L] else as.integer(length(x) > 0L) }
    nn1 <- !is.null(N1 <- if((l1 <- Ncol(..1)) && !ism1) Nms(1)) # else NULL
    nn2 <- !is.null(N2 <- if(na == 2 && Ncol(..2) && !ism2) Nms(2))
    if(nn1 || nn2 || fix.na) {
	if(is.null(colnames(r)))
	    colnames(r) <- rep.int("", ncol(r))
	setN <- function(i, nams)
	    colnames(r)[i] <<- if(is.null(nams)) "" else nams
	if(nn1) setN(1,	 N1)
	if(nn2) setN(1+l1, N2)
	if(fix.na) setN(ncol(r), Nna)
    }
    r
}

## To be active, the above cbind() must "replace" cbind() in "base".
## This may be called on loading methods, see ./zzz.R
bind_activation <- function(on = TRUE)
{
    inBase <- function(x, value, ns)
    {
        unlockBinding(x, ns)
        assign(x, value, envir = ns, inherits = FALSE)
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding(x, ns)
        invisible(NULL)
    }

    ## 'bind' : cbind && rbind
    ## as from 2.4.0 this saving is done in base, so could simplify code
    base.ns <- getNamespace("base")
    saved <- exists(".__H__.cbind", envir = base.ns, inherits = FALSE)
    if(was.on <- saved)
        was.on <- !identical(base::cbind, base::.__H__.cbind)
    if(on) {
        if(!saved) {
            inBase(".__H__.cbind", base::cbind, base.ns)
            inBase(".__H__.rbind", base::rbind, base.ns)
        }
	inBase("cbind", cbind, base.ns)
	inBase("rbind", rbind, base.ns)
    }
    else if(!on && was.on) { ## turn it off
        inBase("cbind", base::.__H__.cbind, ns = base.ns)
        inBase("rbind", base::.__H__.rbind, ns = base.ns)
    }
    was.on
}

### cbind2 () :	 Generic and methods need to be "method-bootstrapped"
### --------   --> ./MethodsListClass.R
