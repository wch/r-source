#### S4-ized  rbind() --- this is entirely parallel to ./cbind() --- KEEP IN SYNC!
###  -------------------- built by
## s/cbind/rbind/ ; s/nrow/N_COL/; s/column/row/; s/colnam/rownam/;
## s/ncol/nrow/ ; s/N_COL/ncol/; s/d[2]/d[1]/

rbind <- function(..., deparse.level = 1)
{
    na <- nargs() - !missing(deparse.level)
    deparse.level <- as.integer(deparse.level)
    stopifnot(0 <= deparse.level, deparse.level <= 2)

    argl <- list(...)
    ## remove trailing 'NULL's:
    while(na > 0 && is.null(argl[[na]])) { argl <- argl[-na]; na <- na - 1 }
    if(na == 0) return(NULL)
    if(na == 1) {
	if(isS4(..1))
	    return(rbind2(..1))
	else return(.Internal(rbind(deparse.level, ...)))
    }

    ## else :  na >= 2

    if(deparse.level) {
	symarg <- as.list(sys.call()[-1])[1:na] # the unevaluated arguments
	## For default 'deparse.level = 1', rbind(a, b) has to give *names*!
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
	## determine ncol(<result>)  for e.g.,	rbind(diag(2), 1, 2)
	## only when the last two argument have *no* dim attribute:
	nrs <- unname(lapply(argl, ncol)) # of length na
	iV <- sapply(nrs, is.null)# is 'vector'
	fix.na <- identical(nrs[(na-1):na], list(NULL,NULL))
	if(fix.na) {
	    ## "fix" last argument, using 1-row `matrix' of proper ncol():
	    nr <- max(if(all(iV)) sapply(argl, length) else unlist(nrs[!iV]))
	    argl[[na]] <- rbind(rep(argl[[na]], length.out = nr),
				deparse.level = 0)
	    ## and since it's a 'matrix' now, rbind() below may not name it
	}
	## need to pass argl, the evaluated arg list to do.call();
	## OTOH, these may have lost their original 'symbols'
	if(deparse.level) {
	    if(fix.na)
		fix.na <- !is.null(Nna <- Nms(na))
	    if(!is.null(nmi <- names(argl))) iV <- iV & (nmi == "")
	    ## attach `symbols' to argl[-1] for 'vectors'[iV]
	    ii <- if(fix.na) # need to fix later ([na] is 'matrix')
		2:(na-1) else 2:na
	    if(any(iV[ii])) {
		for(i in ii[iV[ii]])
		    if (!is.null(nmi <- Nms(i))) names(argl)[i] <- nmi
	    }
	}
	r <- do.call(rbind, c(argl[-1], list(deparse.level=deparse.level)))
    }

    d2 <- dim(r)
    r <- rbind2(..1, r)
    if(deparse.level == 0)
	return(r)
    ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2
    ism2 <- !is.null(d2)	     && length(d2) == 2 && !fix.na
    if(ism1 && ism2) ## two matrices
	return(r)

    ## else -- Setting rownames correctly
    ##	       when one was not a matrix [needs some diligence!]
    Nrow <- function(x) {
	d <- dim(x); if(length(d) == 2) d[1] else as.integer(length(x) > 0) }
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
