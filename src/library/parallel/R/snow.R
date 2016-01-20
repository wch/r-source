#  File src/library/parallel/R/snow.R
#  Part of the R package, https://www.R-project.org
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
#  https://www.R-project.org/Licenses/

## Derived from snow 0.3-6 by Luke Tierney

.reg <-  new.env()
assign("default", NULL, envir = .reg)

defaultCluster <- function(cl = NULL)
{
    if(is.null(cl)) cl <- get("default", envir = .reg)
    if(is.null(cl)) stop("no cluster 'cl' supplied and none is registered")
    checkCluster(cl)
    cl
}

setDefaultCluster <- function(cl = NULL)
{
    if(!is.null(cl)) checkCluster(cl)
    assign("default", cl, envir = .reg)
}

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
    port <- Sys.getenv("R_PARALLEL_PORT")
    port <- if (identical(port, "random")) NA else as.integer(port)
    if (is.na(port)) {
        ran1 <- sample.int(.Machine$integer.max - 1L, 1L) / .Machine$integer.max
        port <- 11000 + 1000 * ((ran1 + unclass(Sys.time()) / 300) %% 1)
    }
    Sys.i <- Sys.info()
    options <- list(port = as.integer(port),
                    timeout = 60 * 60 * 24 * 30, # 30 days
                    master = Sys.i[["nodename"]],
                    homogeneous = TRUE,
                    type = "PSOCK",
                    outfile = "/dev/null",
                    rscript = rscript,
                    rscript_args = character(),
                    user = Sys.i[["user"]],
                    rshcmd = "ssh",
                    manual = FALSE,
                    methods = TRUE,
                    renice = NA_integer_,
                    ## rest are unused in parallel
                    rhome = R.home(),
                    rlibs = Sys.getenv("R_LIBS"),
                    scriptdir = file.path(libname, "parallel"),
                    rprog = file.path(R.home("bin"), "R"),
                    snowlib = .libPaths()[1],
                    useRscript = TRUE, # for use by snow clusters
                    useXDR = TRUE)
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


makeCluster <-
    function (spec, type = getClusterOption("type"), ...)
{
    switch(type,
           PSOCK = makePSOCKcluster(spec, ...),
           FORK = makeForkCluster(spec, ...),
           SOCK = snow::makeSOCKcluster(spec, ...),
           MPI = snow::makeMPIcluster(spec, ...),
           NWS = snow::makeNWScluster(spec, ...),
           stop("unknown cluster type"))
}


stopCluster <- function(cl = NULL)
{
    cl <- defaultCluster(cl)
    if(identical(cl, get("default", envir = .reg)))
        assign("default", NULL, envir = .reg)
    UseMethod("stopCluster")
}

stopCluster.default <- function(cl) for (n in cl) stopNode(n)


#
# Cluster Functions
#

sendCall <- function (con, fun, args, return = TRUE, tag = NULL)
{
    timing <-  .snowTimingData$running()
    if (timing)
        start <- proc.time()[3L]
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

### ========== snow support ===========

## place holder for now.
.snowTimingData <-
    list(running = function() FALSE,
         enterSend = function(...) {},
         enterRecv = function(...) {})


closeNode.NWSnode <- function(node) snow::closeNode.NWSnode(node)

recvData.MPInode <- function(node) snow::recvData.MPInode(node)
recvData.NWSnode <- function(node) snow::recvData.NWSnode(node)

recvOneData.MPIcluster <- function(cl) snow::recvOneData.MPIcluster(cl)
recvOneData.NWScluster <- function(cl) snow::recvOneData.NWScluster(cl)

sendData.MPInode <- function(node, data) snow::sendData.MPInode(node, data)
sendData.NWSnode <- function(node, data) snow::sendData.NWSnode(node, data)

## these use NextMethod() so need copies.
stopCluster.MPIcluster <- function(cl) {
    NextMethod()
    snow::setMPIcluster(NULL)
}

stopCluster.spawnedMPIcluster <- function(cl) {
    comm <- 1
    NextMethod()
    Rmpi::mpi.comm.disconnect(comm)
}

stopCluster.NWScluster <- function(cl) {
    NextMethod()
    nws::nwsDeleteWs(cl[[1]]$wsServer, nws::nwsWsName(cl[[1]]$ws))
    close(cl[[1]]$wsServer)
}

