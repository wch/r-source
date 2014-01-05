#  File src/library/parallel/R/clusterApply.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2013 The R Core Team
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

## Derived from snow 0.3-6 by Luke Tierney

staticClusterApply <- function(cl = NULL, fun, n, argfun) {
    cl <- defaultCluster(cl)
    p <- length(cl)
    if (n > 0L && p) {
        val <- vector("list", n)
        start <- 1L
        while (start <= n) {
            end <- min(n, start + p - 1L)
	    jobs <- end - start + 1L
            for (i in 1:jobs)
                sendCall(cl[[i]], fun, argfun(start + i - 1L))
            val[start:end] <- lapply(cl[1:jobs], recvResult)
            start <- start + jobs
        }
        checkForRemoteErrors(val)
    }
}

dynamicClusterApply <- function(cl = NULL, fun, n, argfun) {
    cl <- defaultCluster(cl)
    p <- length(cl)
    if (n > 0L && p) {
        submit <- function(node, job)
            sendCall(cl[[node]], fun, argfun(job), tag = job)
        for (i in 1:min(n, p)) submit(i, i)
        val <- vector("list", n)
        for (i in 1:n) {
            d <- recvOneResult(cl)
            j <- i + min(n, p)
            if (j <= n) submit(d$node, j)
            val[d$tag] <- list(d$value)
        }
        checkForRemoteErrors(val)
    }
}

## exported and documented from here down unless otherwise stated.

clusterCall  <- function(cl = NULL, fun, ...)
{
    cl <- defaultCluster(cl)
    for (i in seq_along(cl)) sendCall(cl[[i]], fun, list(...))
    checkForRemoteErrors(lapply(cl, recvResult))
}


clusterEvalQ <- function(cl = NULL, expr)
    clusterCall(cl, eval, substitute(expr), env=.GlobalEnv)

clusterExport <- local({
    gets <- function(n, v) { assign(n, v, envir = .GlobalEnv); NULL }
    function(cl = NULL, varlist, envir = .GlobalEnv) {
        ## do this with only one clusterCall--loop on workers?
        for (name in varlist) {
            clusterCall(cl, gets, name, get(name, envir = envir))
        }
    }
})

clusterApply <- function(cl = NULL, x, fun, ...)
{
    ## **** this closure is sending all of x to all nodes
    argfun <- function(i) c(list(x[[i]]), list(...))
    staticClusterApply(cl, fun, length(x), argfun)
}

clusterApplyLB <- function(cl = NULL, x, fun, ...)
{
    ## **** this closure is sending all of x to all nodes
    argfun <- function(i) c(list(x[[i]]), list(...))
    dynamicClusterApply(cl, fun, length(x), argfun)
}

clusterMap <- function (cl = NULL, fun, ..., MoreArgs = NULL, RECYCLE = TRUE,
                        SIMPLIFY = FALSE, USE.NAMES = TRUE,
                        .scheduling = c("static", "dynamic"))
{
    cl <- defaultCluster(cl)
    args <- list(...)
    if (length(args) == 0) stop("need at least one argument")
    .scheduling <- match.arg(.scheduling)
    n <- sapply(args, length)
    if (RECYCLE) {
        vlen <- max(n)
        if(vlen && min(n) == 0L)
            stop("zero-length inputs cannot be mixed with those of non-zero length")
        if (!all(n == vlen))
            for (i in seq_along(args)) # why not lapply?
                args[[i]] <- rep(args[[i]], length.out = vlen)
    }
    else vlen <- min(n)
    ## **** this closure is sending all of ... to all nodes
    argfun <- function(i) c(lapply(args, function(x) x[[i]]), MoreArgs)
    answer <-
        if(.scheduling == "dynamic") dynamicClusterApply(cl, fun, vlen, argfun)
    else staticClusterApply(cl, fun, vlen, argfun)
    ## rest matches mapply(): with a different default for SIMPLIFY
    if (USE.NAMES && length(args)) {
        if (is.null(names1 <- names(args[[1L]])) && is.character(args[[1L]]))
            names(answer) <- args[[1L]]
        else if (!is.null(names1))
            names(answer) <- names1
    }
    if (!identical(SIMPLIFY, FALSE) && length(answer))
        simplify2array(answer, higher = (SIMPLIFY == "array"))
    else answer
}

## splitIndices <- function(nx, ncl)
## {
##     i <- seq_len(nx)
##     if (ncl == 1L) i
##     else structure(split(i, cut(i, ncl)), names = NULL)
## }

# The fuzz used by cut() is too small when nx and ncl are both large
# and causes some groups to be empty. The definition below avoids that
# while minimizing changes from the results produced by the definition
# above.
splitIndices <- function(nx, ncl) {
    i <- seq_len(nx)
    if (ncl == 0L) list()
    else if (ncl == 1L || nx == 1L) list(i)
    else {
        fuzz <- min((nx - 1L) / 1000, 0.4 * nx / ncl)
        breaks <- seq(1 - fuzz, nx + fuzz, length = ncl + 1L)
        structure(split(i, cut(i, breaks)), names = NULL)
    }
}

clusterSplit <- function(cl = NULL, seq) {
    cl <- defaultCluster(cl)
    lapply(splitIndices(length(seq), length(cl)), function(i) seq[i])
}

#internal
splitList <- function(x, ncl)
    lapply(splitIndices(length(x), ncl), function(i) x[i])

#internal
splitRows <- function(x, ncl)
    lapply(splitIndices(nrow(x), ncl), function(i) x[i, , drop=FALSE])

#internal
splitCols <- function(x, ncl)
    lapply(splitIndices(ncol(x), ncl), function(i) x[, i, drop=FALSE])

parLapply <- function(cl = NULL, X, fun, ...)
{
    cl <- defaultCluster(cl)
    do.call(c,
            clusterApply(cl, x = splitList(X, length(cl)),
                         fun = lapply, fun, ...),
            quote = TRUE)
}

parLapplyLB <- function(cl = NULL, X, fun, ...)
{
    cl <- defaultCluster(cl)
    do.call(c,
            clusterApplyLB(cl, x = splitList(X, length(cl)),
                           fun = lapply, fun, ...),
            quote = TRUE)
}

parRapply <- function(cl = NULL, x, FUN, ...)
{
    cl <- defaultCluster(cl)
    do.call(c,
            clusterApply(cl = cl, x = splitRows(x, length(cl)),
                         fun = apply, MARGIN = 1L, FUN = FUN, ...),
            quote = TRUE)
}

parCapply <- function(cl = NULL, x, FUN, ...) {
    cl <- defaultCluster(cl)
    do.call(c,
            clusterApply(cl = cl, x = splitCols(x, length(cl)),
                         fun = apply, MARGIN = 2L, FUN = FUN, ...),
            quote = TRUE)
}


parSapply <-
    function (cl = NULL, X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN) # should this be done on worker?
    answer <- parLapply(cl, X = as.list(X), fun = FUN, ...)
    if(USE.NAMES && is.character(X) && is.null(names(answer)))
	names(answer) <- X
    if(!identical(simplify, FALSE) && length(answer))
	simplify2array(answer, higher = (simplify == "array"))
    else answer
}

parSapplyLB <-
    function (cl = NULL, X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN) # should this be done on worker?
    answer <- parLapplyLB(cl, X = as.list(X), fun = FUN, ...)
    if(USE.NAMES && is.character(X) && is.null(names(answer)))
	names(answer) <- X
    if(!identical(simplify, FALSE) && length(answer))
	simplify2array(answer, higher = (simplify == "array"))
    else answer
}


parApply <- function(cl = NULL, X, MARGIN, FUN, ...)
{
    cl <- defaultCluster(cl) # initial sanity check
    FUN <- match.fun(FUN) # should this be done on worker?

    ## Ensure that X is an array object
    dl <- length(dim(X))
    if(!dl) stop("dim(X) must have a positive length")
    if(is.object(X))
	X <- if(dl == 2L) as.matrix(X) else as.array(X)
    ## now record dim as coercion can change it
    ## (e.g. when a data frame contains a matrix).
    d <- dim(X)
    dn <- dimnames(X)
    ds <- seq_len(dl)

    ## Extract the margins and associated dimnames

    if (is.character(MARGIN)) {
        if(is.null(dnn <- names(dn))) # names(NULL) is NULL
           stop("'X' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if (anyNA(MARGIN))
            stop("not all elements of 'MARGIN' are names of dimensions")
    }
    s.call <- ds[-MARGIN]
    s.ans  <- ds[MARGIN]
    d.call <- d[-MARGIN]
    d.ans  <- d[MARGIN]
    dn.call <- dn[-MARGIN]
    dn.ans <- dn[MARGIN]
    ## dimnames(X) <- NULL

    ## do the calls

    d2 <- prod(d.ans)
    if(d2 == 0L) {
        ## arrays with some 0 extents: return ``empty result'' trying
        ## to use proper mode and dimension:
        ## The following is still a bit `hackish': use non-empty X
        newX <- array(vector(typeof(X), 1L), dim = c(prod(d.call), 1L))
        ans <- FUN(if(length(d.call) < 2L) newX[,1] else
                   array(newX[, 1L], d.call, dn.call), ...)
        return(if(is.null(ans)) ans else if(length(d.ans) < 2L) ans[1L][-1L]
               else array(ans, d.ans, dn.ans))
    }
    ## else
    newX <- aperm(X, c(s.call, s.ans))
    dim(newX) <- c(prod(d.call), d2)
    ans <- vector("list", d2)
    arglist <- if(length(d.call) < 2L) {# vector
        if (length(dn.call)) dimnames(newX) <- c(dn.call, list(NULL))
        lapply(seq_len(d2), function(i) newX[,i])
    } else
        lapply(seq_len(d2), function(i) array(newX[,i], d.call, dn.call))
    ans <- parLapply(cl = cl, X = arglist, fun = FUN, ...)

    ## answer dims and dimnames

    ans.list <- is.recursive(ans[[1L]])
    l.ans <- length(ans[[1L]])

    ans.names <- names(ans[[1L]])
    if(!ans.list)
	ans.list <- any(unlist(lapply(ans, length)) != l.ans)
    if(!ans.list && length(ans.names)) {
        all.same <- vapply(ans, function(x) identical(names(x), ans.names), NA)
        if (!all(all.same)) ans.names <- NULL
    }
    len.a <- if(ans.list) d2 else length(ans <- unlist(ans, recursive = FALSE))
    if(length(MARGIN) == 1L && len.a == d2) {
	names(ans) <- if(length(dn.ans[[1L]])) dn.ans[[1L]] # else NULL
	return(ans)
    }
    if(len.a == d2)
	return(array(ans, d.ans, dn.ans))
    if(len.a && len.a %% d2 == 0L) {
        if(is.null(dn.ans)) dn.ans <- vector(mode="list", length(d.ans))
        dn.ans <- c(list(ans.names), dn.ans)
	return(array(ans, c(len.a %/% d2, d.ans),
		     if(!all(vapply(dn.ans, is.null, NA))) dn.ans))
    }
    return(ans)
}

