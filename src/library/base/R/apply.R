apply <- function(X, MARGIN, FUN, ...)
{
    FUN <- match.fun(FUN)

    ## Ensure that X is an array object
    d <- dim(X)
    dl <- length(d)
    if(dl == 0)
	stop("dim(X) must have a positive length")
    ds <- 1:dl
    if(length(class(X)) > 0)
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

    newX <- aperm(X, c(s.call, s.ans))
    dim(newX) <- c(prod(d.call), d2 <- prod(d.ans))

    ans <- vector("list", d2)
    if((i.vec <- length(d.call) < 2)) # vector
	for(i in 1:d2){
	    xi <- newX[,i]
	    if (length(dn.call) >= 1)
		names(xi) <- dn.call[[1]]
	    ans[[i]] <- FUN(xi, ...)
	}
    else
	for(i in 1:d2) ans[[i]] <- FUN(array(newX[,i], d.call, dn.call), ...)

    ## answer dims and dimnames

    ans.list <- is.recursive(ans[[1]])
    l.ans <- length(ans[[1]])

    ans.names <- names(ans[[1]])
    if(i.vec && is.null(ans.names) && length(dn.call) &&
       l.ans == length(an <- dn.call[[1]]))
	ans.names <- an
    if(!ans.list)
	ans.list <- any(unlist(lapply(ans, length)) != l.ans)
    len.a <- if(ans.list) d2 else length(ans <- unlist(ans, recursive = FALSE))
    if(length(MARGIN) == 1 && len.a == d2) {
	names(ans) <- if(length(dn.ans[[1]]) > 0) dn.ans[[1]] # else NULL
	return(ans)
    }
    if(len.a == d2)
	return(array(ans, d.ans, dn.ans))
    if(len.a > 0 && len.a %% d2 == 0)
	return(array(ans, c(len.a %/% d2, d.ans),
		     dimnames = if(is.null(dn.ans)) list(ans.names,NULL)
		     else c(list(ans.names), dn.ans)))
    return(ans)
}
