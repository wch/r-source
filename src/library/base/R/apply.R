apply <- function(X, MARGIN, FUN, ...)
{
    FUN <- match.fun(FUN)

    ## Ensure that X is an array object
    d <- dim(X)
    dl <- length(d)
    if(dl == 0)
	stop("dim(X) must have a positive length")
    ds <- 1:dl
    if(length(oldClass(X)) > 0)
	X <- if(dl == 2) as.matrix(X) else as.array(X)
    dn <- dimnames(X)

    ## Extract the margins and associated dimnames

    s.call <- ds[-MARGIN]
    s.ans  <- ds[MARGIN]
    d.call <- d[-MARGIN]
    d.ans  <- d[MARGIN]
    dn.call<- dn[-MARGIN]
    dn.ans <- dn[MARGIN]
    ## dimnames(X) <- NULL

    ## do the calls

    d2 <- prod(d.ans)
    if(d2 == 0) {
        ## arrays with some 0 extents: return ``empty result'' trying
        ## to use proper mode and dimension:
        ## The following is still a bit `hackish': use non-empty X
        newX <- array(vector(typeof(X), 1), dim = c(prod(d.call), 1))
        ans <- FUN(if(length(d.call) < 2) newX[,1] else
                   array(newX[,1], d.call, dn.call), ...)
        return(if(is.null(ans)) ans else if(length(d.call) < 2) ans[1][-1]
               else array(ans, d.ans, dn.ans))
    }
    ## else
    newX <- aperm(X, c(s.call, s.ans))
    dim(newX) <- c(prod(d.call), d2)
    ans <- vector("list", d2)
    if(length(d.call) < 2) {# vector
        if (length(dn.call)) dimnames(newX) <- c(dn.call, list(NULL))
        for(i in 1:d2) ans[[i]] <- FUN(newX[,i], ...)
    } else
       for(i in 1:d2) ans[[i]] <- FUN(array(newX[,i], d.call, dn.call), ...)

    ## answer dims and dimnames

    ans.list <- is.recursive(ans[[1]])
    l.ans <- length(ans[[1]])

    ans.names <- names(ans[[1]])
    if(!ans.list)
	ans.list <- any(unlist(lapply(ans, length)) != l.ans)
    if(!ans.list && length(ans.names)) {
        all.same <- sapply(ans, function(x) identical(names(x), ans.names))
        if (!all(all.same)) ans.names <- NULL
    }
    len.a <- if(ans.list) d2 else length(ans <- unlist(ans, recursive = FALSE))
    if(length(MARGIN) == 1 && len.a == d2) {
	names(ans) <- if(length(dn.ans[[1]])) dn.ans[[1]] # else NULL
	return(ans)
    }
    if(len.a == d2)
	return(array(ans, d.ans, dn.ans))
    if(len.a > 0 && len.a %% d2 == 0) {
        if(is.null(dn.ans)) dn.ans <- vector(mode="list", length(d.ans))
        dn.ans <- c(list(ans.names), dn.ans)
	return(array(ans, c(len.a %/% d2, d.ans),
                     if(!all(sapply(dn.ans, is.null))) dn.ans))
    }
    return(ans)
}
