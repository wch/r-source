#  File src/library/parallel/R/snowSOCK.R
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

## Derived from snow 0.3-6 by Luke Tierney
## Uses solely Rscript, and a function in the package rather than scripts.

newPSOCKnode <- function(machine = "localhost", ...,
                         options = defaultClusterOptions, rank)
{
    options <- addClusterOptions(options, list(...))
    if (is.list(machine)) {
        options <- addClusterOptions(options, machine)
        machine <- machine$host
    }
    outfile <- getClusterOption("outfile", options)
    master <- if (machine == "localhost") "localhost"
    else getClusterOption("master", options)
    port <- getClusterOption("port", options)
    manual <- getClusterOption("manual", options)
    timeout <- getClusterOption("timeout", options)
    methods <- getClusterOption("methods", options)
    useXDR <- getClusterOption("useXDR", options)

    ## build the local command for starting the worker
    env <- paste0("MASTER=", master,
                 " PORT=", port,
                 " OUT=", outfile,
                 " TIMEOUT=", timeout,
                 " METHODS=", methods,
                 " XDR=", useXDR)
    arg <- "parallel:::.slaveRSOCK()"
    rscript <- if (getClusterOption("homogeneous", options)) {
        shQuote(getClusterOption("rscript", options))
    } else "Rscript"
    rscript_args <- getClusterOption("rscript_args", options)

    ## in principle we should quote these,
    ## but the current possible values do not need quoting
    cmd <- if(length(rscript_args))
        paste(rscript, paste(rscript_args, collapse = " "),
              "-e", shQuote(arg), env)
    else paste(rscript, "-e", shQuote(arg), env)

    ## We do redirection of connections at R level once the process is
    ## running.  We could instead do it at C level here, at least on
    ## a Unix-alike.
    renice <- getClusterOption("renice", options)
    if(!is.na(renice) && renice) ## ignore 0
        cmd <- sprintf("nice +%d %s", as.integer(renice), cmd)

    if (manual) {
        cat("Manually start worker on", machine, "with\n    ", cmd, "\n")
        utils::flush.console()
    } else {
        ## add the remote shell command if needed
        if (machine != "localhost") {
            ## This assumes an ssh-like command
            rshcmd <- getClusterOption("rshcmd", options)
            user <- getClusterOption("user", options)
            ## this assume that rshcmd will use a shell, and that is
            ## the same shell as on the master.
            cmd <- shQuote(cmd)
            cmd <- paste(rshcmd, "-l", user, machine, cmd)
        }

        if (.Platform$OS.type == "windows") {
            ## snow said:
            ## On Windows using input = something seems needed to
            ## disconnect standard input of an ssh process when run
            ## from Rterm (at least using putty's plink).  In
            ## principle this could also be used for supplying a
            ## password, but that is probably a bad idea. So, for now
            ## at least, on Windows password-less authentication is
            ## necessary.
            ##
            ## (Not clear if that is the current behaviour: works for me)
            system(cmd, wait = FALSE, input = "")
        }
        else system(cmd, wait = FALSE)
    }

    con <- socketConnection("localhost", port = port, server = TRUE,
                            blocking = TRUE, open = "a+b", timeout = timeout)
    structure(list(con = con, host = machine, rank = rank),
              class = if(useXDR) "SOCKnode" else "SOCK0node")
}

closeNode.SOCKnode <- closeNode.SOCK0node <- function(node) close(node$con)

sendData.SOCKnode <- function(node, data) serialize(data, node$con)
sendData.SOCK0node <- function(node, data) serialize(data, node$con, xdr = FALSE)

recvData.SOCKnode <- recvData.SOCK0node <- function(node) unserialize(node$con)

recvOneData.SOCKcluster <- function(cl)
{
    socklist <- lapply(cl, function(x) x$con)
    repeat {
        ready <- socketSelect(socklist)
        if (length(ready) > 0) break;
    }
    n <- which.max(ready) # may need rotation or some such for fairness
    list(node = n, value = unserialize(socklist[[n]]))
}

makePSOCKcluster <- function(names, ...)
{
    if (is.numeric(names)) {
        names <- as.integer(names[1L])
        if(is.na(names) || names < 1L) stop("numeric 'names' must be >= 1")
        names <- rep('localhost', names)
    }
    .check_ncores(length(names))
    options <- addClusterOptions(defaultClusterOptions, list(...))
    cl <- vector("list", length(names))
    for (i in seq_along(cl))
        cl[[i]] <- newPSOCKnode(names[[i]], options = options, rank = i)
    class(cl) <- c("SOCKcluster", "cluster")
    cl
}

print.SOCKcluster <- function(x, ...)
{
    nc <- length(x)
    hosts <- unique(sapply(x, "[[", "host"))
    msg <- sprintf(ngettext(length(hosts),
                            "socket cluster with %d nodes on host %s",
                            "socket cluster with %d nodes on hosts %s"),
                   nc, paste(sQuote(hosts), collapse = ", "))
    cat(msg, "\n", sep = "")
    invisible(x)
}

print.SOCKnode <- print.SOCK0node <- function(x, ...)
{
    sendCall(x, eval, list(quote(Sys.getpid())))
    pid <- recvResult(x)

    msg <- gettextf("node of a socket cluster on host %s with pid %d",
                    sQuote(x[["host"]]), pid)
    cat(msg, "\n", sep = "")
    invisible(x)
}

.slaveRSOCK <- function()
{
    makeSOCKmaster <- function(master, port, timeout, useXDR)
    {
        port <- as.integer(port)
        ## maybe use `try' and sleep/retry if first time fails?
        con <- socketConnection(master, port = port, blocking = TRUE,
                                open = "a+b", timeout = timeout)
        structure(list(con = con),
                  class = if(useXDR) "SOCKnode" else "SOCK0node")
    }

    ## set defaults in case run manually without args.
    master <- "localhost"
    port <- NA_integer_ # no point in getting option on worker.
    outfile <- Sys.getenv("R_SNOW_OUTFILE") # defaults to ""
    methods <- TRUE
    useXDR <- TRUE

    for (a in commandArgs(TRUE)) {
        ## Or use strsplit?
        pos <- regexpr("=", a)
        name <- substr(a, 1L, pos - 1L)
        value <- substr(a, pos + 1L, nchar(a))
        switch(name,
               MASTER = {master <- value},
               PORT = {port <- value},
               OUT = {outfile <- value},
               TIMEOUT = {timeout <- value},
               METHODS = {methods <- value},
               XDR = {useXDR <- as.logical(value)})
    }
    if (is.na(port)) stop("PORT must be specified")

    if(as.logical(methods)) library("methods") ## because Rscript does not load methods by default
    ## We should not need to attach parallel, as we are running in the namespace.

    sinkWorkerOutput(outfile)
    msg <- sprintf("starting worker pid=%d on %s at %s\n",
                   Sys.getpid(), paste(master, port, sep = ":"),
                   format(Sys.time(), "%H:%M:%OS3"))
    cat(msg)
    slaveLoop(makeSOCKmaster(master, port, timeout, useXDR))
}
