
### Experimental {similar c() code: MM's ~/R/MM/Pkg-ex/methods/cbind-etc.R}

## Note that this
## 1) is namespace-hidden usually,
## 2) cbind / rbind are almost never called in 'methods' itself
## hence, the following has almost no effect unless ``activated'' (see below)
cbind <- function(..., deparse.level = 1)
{
    na <- nargs() - !missing(deparse.level)
    deparse.level <- as.integer(deparse.level)
    stopifnot(0 <= deparse.level, deparse.level <= 2)

    if(na <= 1) return(.Internal(cbind(deparse.level, ...)))

    ## else :
    larg <- as.list(sys.call()[-1])
    ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2
    if(na == 2) {
        r <- ..2
    } else { ## >= 3 arguments: recurse but
        ## should find nrow(<result>)  for e.g.,  cbind(diag(2), 1, 2)
        nrow <- -1
        ## for(a in list(...)[-1]) if(!is.null(dim(a)))
        r <- do.call(cbind, c(list(...)[-1], list(deparse.level=deparse.level)))
    }

    ism2 <- !is.null(d2 <- dim( r )) && length(d2) == 2
    r <- cbind2(..1, r)
    ## NOTE: We rely on
    ##
    ## o   dim(.) working reliably for all arguments
    ## o   All cbind2() methods are assumed to
    ##	       1) correctly set rownames
    ##	       2) correctly set colnames for *matrix*(like) arguments

    if(ism1 && ism2 || deparse.level == 0)# two matrices or no 'deparse naming'
	return(r)

    ## else -- all the rest: Setting colnames correctly
    ##	       when one was not a matrix [needs some diligence!]

    Ncol <- function(x) {
	d <- dim(x); if(length(d) == 2) d[2] else as.integer(length(x) > 0) }
    ## For default 'deparse.level = 1',	  cbind(a, b) has to give *names*!
    Nms <- function(i) { # possibly 'deparsed' names of argument  i
	if(is.null(r <- names(larg[i])) || r == "") {
	    if(is.symbol(r <- larg[[i]]) || deparse.level == 2)
		deparse(r) # else NULL
	} else r
    }

    nn1 <- !is.null(N1 <- if((l1 <- Ncol(..1)) && !ism1) Nms(1)) # else NULL
    nn2 <- !is.null(N2 <- if(na == 2 && Ncol(..2) && !ism2) Nms(2))
    if(nn1 || nn2) {
	if(is.null(colnames(r)))
	    colnames(r) <- rep.int("", ncol(r))
        setN <- function(i0, nams)
            colnames(r)[i0+1] <<- if(is.null(nams)) "" else nams
	if(nn1) setN(0,	 N1)
	if(nn2) setN(l1, N2)
    }
    r
}

## To be active, the above cbind() must "replace" cbind() in "base" :
bind_activation <- function(on = TRUE) {
    ## 'bind' : cbind && rbind (eventually)
    base.ns <- getNamespace("base")
    if(on) {
	utils::assignInNamespace(".__H__.cbind", base::cbind, ns = base.ns)
	utils::assignInNamespace("cbind", cbind, ns = base.ns)
    }
    else { # turn it off
	if(!inherits(try(getFromNamespace(".__H__.cbind", ns = base.ns),
			 silent = TRUE), "try-error"))
	    utils::assignInNamespace("cbind", base::.__H__.cbind, ns = base.ns)
    }
}

### cbind2 () :	 Generic and methods need to be "method-bootstrapped"
### --------   --> ./MethodsListClass.R
