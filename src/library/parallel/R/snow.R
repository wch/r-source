#  File src/library/parallel/R/snow.R
#  Part of the R package, http://www.R-project.org
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

## Extracted from snow 0.3-6.


#
# Checking and subsetting
#

checkCluster <- function(cl)
    if (!inherits(cl, "cluster")) stop("not a valid cluster");

`[.cluster` <- function(cl, ...) {
    v <- NextMethod()
    class(v) <- class(cl)
    v
}


#
# Slave Loop Functions, used in .slaveRSOCK
#

slaveLoop <- function(master)
{
    repeat
        tryCatch({
            msg <- recvData(master)
            cat(paste("Type:", msg$type, "\n"))

            if (msg$type == "DONE") {
                closeNode(master)
                break;
            }
            else if (msg$type == "EXEC") {
                success <- TRUE
                ## This uses the message, rather than the exception since
                ## the exception class/methods may not be available on the
                ## master.
                handler <- function(e) {
                    success <<- FALSE
                    structure(conditionMessage(e),
                              class=c("snow-try-error","try-error"))
                }
                t1 <- proc.time()
                value <- tryCatch(do.call(msg$data$fun, msg$data$args, quote = TRUE),
                                  error = handler)
                t2 <- proc.time()
                value <- list(type = "VALUE", value = value, success = success,
                              time = t2 - t1, tag = msg$data$tag)
                sendData(master, value)
            }
        }, interrupt = function(e) NULL)
}

sinkWorkerOutput <- function(outfile)
{
    if (nzchar(outfile)) {
        if (.Platform$OS.type == "windows" && outfile == "/dev/null")
            outfile <- "nul:"
        outcon <- file(outfile, open = "w")
        sink(outcon)
        sink(outcon, type = "message")
    }
}


#
# Higher-Level Node Functions
#

closeNode <- function(node) UseMethod("closeNode")
closeNode.default <- function(node) {}

## These have SOCK methods
sendData <- function(node, data) UseMethod("sendData")
recvData <- function(node) UseMethod("recvData")
recvOneData <- function(cl) UseMethod("recvOneData")

postNode <- function(con, type, value = NULL, tag = NULL)
    sendData(con, list(type = type, data = value, tag = tag))

stopNode <- function(n) {
    postNode(n, "DONE")
    closeNode(n)
}



#
#  Cluster Creation and Destruction
#

defaultClusterOptions <- NULL

#**** check valid cluster option

initDefaultClusterOptions <- function(libname)
{
    rscript <- file.path(R.home("bin"), "Rscript")
    options <- list(port = 10187,
                    timeout = 60 * 60 * 24 * 365, # one year
                    master =  Sys.info()["nodename"],
                    homogeneous = TRUE,
                    type = "PSOCK",
                    outfile = "/dev/null",
                    rscript = rscript,
                    user = Sys.info()["user"],
                    rshcmd = "ssh",
                    manual = FALSE,
                    ## rest are unused in parallel
                    rhome = R.home(),
                    rlibs = Sys.getenv("R_LIBS"),
                    scriptdir = file.path(libname, "parallel"),
                    rprog = file.path(R.home("bin"), "R"),
                    snowlib = .libPaths()[1], # unused in parallel
                    useRscript = TRUE)
    defaultClusterOptions <<- addClusterOptions(emptyenv(), options)
}

addClusterOptions <- function(options, new) {
    if (!is.null(new)) {
        options <- new.env(parent = options)
        names <- names(new)
        for (i in seq_along(new))
            assign(names[i], new[[i]], envir = options)
    }
    options
}

getClusterOption <- function(name, options = defaultClusterOptions)
    get(name, envir = options)

setDefaultClusterOptions <- function(...) {
    list <- list(...)
    names <- names(list)
    for (i in seq_along(list))
        assign(names[i], list[[i]], envir = defaultClusterOptions)
}

stopCluster <- function(cl) UseMethod("stopCluster")

stopCluster.default <- function(cl) for (n in cl) stopNode(n)


#
# Cluster Functions
#

sendCall <- function (con, fun, args, return = TRUE, tag = NULL)
{
    timing <-  .snowTimingData$running()
    postNode(con, "EXEC",
             list(fun = fun, args = args, return = return, tag = tag))
    if (timing)
        .snowTimingData$enterSend(con$rank, start, proc.time()[3L])
    NULL
}

recvResult <- function(con)
{
    if (.snowTimingData$running()) {
        start <- proc.time()[3L]
        r <- recvData(con)
        end <- proc.time()[3L]
        .snowTimingData$enterRecv(con$rank, start, end, r$time[3L])
    }
    else r <- recvData(con)
    r$value
}

checkForRemoteErrors <- function(val)
{
    count <- 0
    firstmsg <- NULL
    for (v in val) {
        if (inherits(v, "try-error")) {
            count <- count + 1
            if (count == 1) firstmsg <- v
        }
    }
    ## These will not translate
    if (count == 1)
        stop("one node produced an error: ", firstmsg, domain = NA)
    else if (count > 1)
        stop(count, " nodes produced errors; first error: ", firstmsg, domain = NA)
    val
}

clusterCall  <- function(cl, fun, ...) {
    checkCluster(cl)
    for (i in seq_along(cl))
        sendCall(cl[[i]], fun, list(...))
    checkForRemoteErrors(lapply(cl, recvResult))
}

staticClusterApply <- function(cl, fun, n, argfun) {
    checkCluster(cl)
    p <- length(cl)
    if (n > 0L && p > 0L) {
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

clusterApply <- function(cl, x, fun, ...) {
    argfun <- function(i) c(list(x[[i]]), list(...))
    staticClusterApply(cl, fun, length(x), argfun)
}

clusterEvalQ <- function(cl, expr)
    clusterCall(cl, eval, substitute(expr), env=.GlobalEnv)

clusterExport <- local({
    gets <- function(n, v) { assign(n, v, envir = .GlobalEnv); NULL }
    function(cl, list) {
        ## do this with only one clusterCall--loop on slaves?
        for (name in list) {
            clusterCall(cl, gets, name, get(name, envir = .GlobalEnv))
        }
    }
})

recvOneResult <- function (cl) {
    if (.snowTimingData$running()) {
        start <- proc.time()[3]
        v <- recvOneData(cl)
        end <- proc.time()[3]
        .snowTimingData$enterRecv(v$node, start, end, v$value$time[3])
    }
    else v <- recvOneData(cl)
    list(value = v$value$value, node = v$node, tag = v$value$tag)
}

findRecvOneTag <- function(cl, anytag) {
    rtag <- NULL
    for (node in cl) {
        if (is.null(rtag))
            rtag <- node$RECVTAG
        else if (rtag != node$RECVTAG) {
            rtag <- anytag
            break;
        }
    }
    rtag
}

dynamicClusterApply <- function(cl, fun, n, argfun) {
    checkCluster(cl)
    p <- length(cl)
    if (n > 0 && p > 0) {
        submit <- function(node, job)
            sendCall(cl[[node]], fun, argfun(job), tag = job)
        for (i in 1 : min(n, p))
            submit(i, i)
        val <- vector("list", n)
        for (i in 1:n) {
            d <- recvOneResult(cl)
            j <- i + min(n, p)
            if (j <= n)
                submit(d$node, j)
            val[d$tag] <- list(d$value)
        }
        checkForRemoteErrors(val)
    }
}

clusterApplyLB <- function(cl, x, fun, ...) {
    ## **** this closure is sending all of x to all nodes
    argfun <- function(i) c(list(x[[i]]), list(...))
    dynamicClusterApply(cl, fun, length(x), argfun)
}

## **** should this allow load balancing?
## **** disallow recycling if one arg is length zero?
clusterMap <- function (cl, fun, ..., MoreArgs = NULL, RECYCLE = TRUE) {
    checkCluster(cl)
    args <- list(...)
    if (length(args) == 0)
        stop("need at least one argument")
    n <- sapply(args, length)
    if (RECYCLE) {
        vlen <- max(n)
        if (!all(n == vlen))
            for (i in 1:length(args)) args[[i]] <- rep(args[[i]],
                length = max(n))
    }
    else vlen = min(n)
    ## **** this closure is sending all of ... to all nodes
    argfun <- function(i) c(lapply(args, function(x) x[[i]]), MoreArgs)
    staticClusterApply(cl, fun, vlen, argfun)
}


#
# Parallel Functions
#

splitIndices <- function(nx, ncl) {
    batchsize <- if (nx %% ncl == 0) nx %/% ncl else 1 + nx %/% ncl
    batches <- (nx + batchsize - 1) %/% batchsize
    split(1:nx, rep(1:batches, each = batchsize)[1:nx])
}

splitIndices <- function(nx, ncl) {
    i <- 1:nx;
    if (ncl == 1L) i
    else structure(split(i, cut(i, ncl)), names=NULL)
}

clusterSplit <- function(cl, seq)
    lapply(splitIndices(length(seq), length(cl)), function(i) seq[i])

splitList <- function(x, ncl)
    lapply(splitIndices(length(x), ncl), function(i) x[i])

splitRows <- function(x, ncl)
    lapply(splitIndices(nrow(x), ncl), function(i) x[i, , drop=FALSE])

splitCols <- function(x, ncl)
    lapply(splitIndices(ncol(x), ncl), function(i) x[, i, drop=FALSE])

parLapply <- function(cl, X, fun, ...)
    do.call(c,
            clusterApply(cl, splitList(X, length(cl)), lapply, fun, ...),
            quote = TRUE)

parRapply <- function(cl, x, fun, ...)
    do.call(c,
            clusterApply(cl, splitRows(x,length(cl)), apply, 1L, fun, ...),
            quote = TRUE)

parCapply <- function(cl, x, fun, ...)
    do.call(c,
            clusterApply(cl, splitCols(x,length(cl)), apply, 2L, fun, ...),
            quote = TRUE)


parSapply <- function (cl, X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE)
{
    FUN <- match.fun(FUN) # should this be done on worker?
    answer <- parLapply(cl,as.list(X), FUN, ...)
    if(USE.NAMES && is.character(X) && is.null(names(answer)))
	names(answer) <- X
    if(!identical(simplify, FALSE) && length(answer))
	simplify2array(answer, higher = (simplify == "array"))
    else answer
}

parApply <- function(cl, X, MARGIN, FUN, ...)
{
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
        if (any(is.na(MARGIN)))
            stop("not all elements of 'MARGIN' are names of dimensions")
    }
    s.call <- ds[-MARGIN]
    s.ans  <- ds[MARGIN]
    d.call <- d[-MARGIN]
    d.ans  <- d[MARGIN]
    dn.call<- dn[-MARGIN]
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
    if(length(d.call) < 2L) {# vector
        if (length(dn.call)) dimnames(newX) <- c(dn.call, list(NULL))
        for(i in 1L:d2) {
            tmp <- FUN(newX[,i], ...)
            if(!is.null(tmp)) ans[[i]] <- tmp
        }
    } else
       for(i in 1L:d2) {
           tmp <- FUN(array(newX[,i], d.call, dn.call), ...)
           if(!is.null(tmp)) ans[[i]] <- tmp
        }

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

### ========== snow support ===========

## place holder for now.
.snowTimingData <-
    list(running = function() FALSE,
         enterSend = function(...) {},
         enterRecv = function(...) {})


closeNode.NWSnode <- function(node) snow::closeNode.NWSnode(node)

recvData.MPInode <- function(node) snow::recvData.MPInode(node)
recvData.NWSnode <- function(node) snow::recvData.NWSnode(node)
recvData.PVMnode <- function(node) snow::recvData.PVMnode(node)

recvOneData.MPIcluster <- function(cl) snow::recvOneData.MPIclusted(cl)
recvOneData.NWScluster <- function(cl) snow::recvOneData.NWScluster(cl)
recvOneData.PVMcluster <- function(cl) snow::recvOneData.PVMcluster(cl)

sendData.MPInode <- function(node, data) snow::sendData.MPInode(node, data)
sendData.NWSnode <- function(node, data) snow::sendData.NWSnode(node, data)
sendData.PVMnode <- function(node, data) snow::sendData.PVMnode(node, data)

stopCluster.MPIcluster <- function(cl) snow::stopCluster.MPIcluster(cl)
stopCluster.NWScluster <- function(cl) snow::stopCluster.NWScluster(cl)
stopCluster.PVMcluster <- function(cl) snow::stopCluster.PVMcluster(cl)
stopCluster.spawnedMPIcluster <-
    function(cl) snow::stopCluster.spawnedMPIcluster(cl)

