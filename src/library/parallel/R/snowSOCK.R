#  File src/library/parallel/R/snowSOCK.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2024 The R Core Team
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
## Uses solely Rscript, and a function in the package rather than scripts.

## NB: there is also workCommand in worker.R: this one is run on the master
workerCommand <- function(machine, options, setup_strategy = "sequential")
{
    outfile <- getClusterOption("outfile", options)
    master <- if (machine == "localhost") "localhost"
              else getClusterOption("master", options)
    port <- getClusterOption("port", options)
    setup_timeout <- getClusterOption("setup_timeout", options)
    manual <- getClusterOption("manual", options)
    timeout <- getClusterOption("timeout", options)
    methods <- getClusterOption("methods", options)
    useXDR <- getClusterOption("useXDR", options)
    homogeneous <- getClusterOption("homogeneous", options)

    ## build the local command for starting the worker
    env <- paste0("MASTER=", master,
                 " PORT=", port,
                 " OUT=", shQuote(outfile),
                 " SETUPTIMEOUT=", setup_timeout,
                 " TIMEOUT=", timeout,
                 " XDR=", useXDR,
                 " SETUPSTRATEGY=", setup_strategy)
    ## Should cmd be run on a worker with R <= 4.0.2,
    ## .workRSOCK will not exist, so fallback to .slaveRSOCK
    arg <- "tryCatch(parallel:::.workRSOCK,error=function(e)parallel:::.slaveRSOCK)()"
    ## option rscript got set by initDefaultClusterOptions to the full path
    ## on the master, but can be overridden in the makePSOCKcluster call.
    rscript <-
        if (homogeneous) shQuote(getClusterOption("rscript", options)) else "Rscript"
    rscript_args <- getClusterOption("rscript_args", options)
    if(methods)
        rscript_args <-c("--default-packages=datasets,utils,grDevices,graphics,stats,methods",
                         rscript_args)

    ## in principle we should quote these,
    ## but the current possible values do not need quoting
    cmd <- paste(rscript,
                 if(length(rscript_args)) paste(rscript_args, collapse = " "),
                 "-e", shQuote(arg), env)

    ## We do redirection of connections at R level once the process is
    ## running.  We could instead do it at C level here, at least on
    ## a Unix-alike.
    renice <- getClusterOption("renice", options)
    if(!is.na(renice) && renice) ## ignore 0
        cmd <- sprintf("nice +%d %s", as.integer(renice), cmd)

    ## add the remote shell command if needed
    if (!manual && machine != "localhost") {
        ## This assumes an ssh-like command
        rshcmd <- getClusterOption("rshcmd", options)
        user <- getClusterOption("user", options)
        ## this assume that rshcmd will use a shell, and that is
        ## the same shell as on the master.
        cmd <- paste(rshcmd,
                     if(length(user) == 1L) paste("-l", user),
                     machine, shQuote(cmd))
    }
    cmd
}

newPSOCKnode <- function(machine = "localhost", ...,
                         options = defaultClusterOptions, rank)
{
    options <- addClusterOptions(options, list(...))
    if (is.list(machine)) {
        options <- addClusterOptions(options, machine)
        machine <- machine$host
    }
    port <- getClusterOption("port", options)
    manual <- getClusterOption("manual", options)
    timeout <- getClusterOption("timeout", options)
    useXDR <- getClusterOption("useXDR", options)
    cmd <- workerCommand(machine, options)

    if (manual) {
        cat("Manually start worker on", machine, "with\n    ", cmd, "\n")
        utils::flush.console()
    } else {
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
            system(cmd, wait = FALSE, input = "",
                   receive.console.signals = TRUE)
        }
        else {
            ## If workers are running a different R version, avoid a WARNING
            cmd <- paste("R_HOME=", cmd)
            system(cmd, wait = FALSE, receive.console.signals = TRUE)
        }
    }

    con <- socketConnection("localhost", port = port, server = TRUE,
                            blocking = TRUE, open = "a+b", timeout = timeout)
    structure(list(con = con, host = machine, rank = rank),
              class = if(useXDR) "SOCKnode" else "SOCK0node")
}

closeNode.SOCKnode <- closeNode.SOCK0node <- function(node)
{
  if ("host" %in% names(node) && "rank" %in% names(node)) 
      close(node$con)

  ## Let the OS close the connection to the master node (see stopCluster)
  ## when a worker finishes.
}

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
    options <- addClusterOptions(defaultClusterOptions, list(...))
    manual <- getClusterOption("manual", options)
    homogeneous <- getClusterOption("homogeneous", options)
    setup_strategy <- match.arg(getClusterOption("setup_strategy",
                                                  options),
                                c("sequential", "parallel"))
    setup_timeout <- getClusterOption("setup_timeout", options)
    local <- is.numeric(names) || (is.character(names) &&
                 identical(names, rep('localhost', length(names))))
    if (is.numeric(names)) {
        names <- as.integer(names[1L])
        if(is.na(names) || names < 1L) stop("numeric 'names' must be >= 1")
        names <- rep('localhost', names)
    }
    .check_ncores(length(names))

    cl <- vector("list", length(names))
    if (!manual && homogeneous && local && setup_strategy == "parallel") {
        port <- getClusterOption("port", options)
        timeout <- getClusterOption("timeout", options)
        useXDR <- getClusterOption("useXDR", options)
        cmd <- workerCommand("localhost", options,
                             setup_strategy = "parallel" )

        ## Start listening and start workers.
        socket <- serverSocket(port = port)
        on.exit(close(socket), add = TRUE)
        if (.Platform$OS.type == "windows") {
            for(i in seq_along(cl))
                ## see newPSOCKnode for the input = ""
                system(cmd, wait = FALSE, input = "",
                       receive.console.signals = TRUE)
        } else {
            ## Asynchronous lists are defined by POSIX
            cmd <- paste(rep(cmd, length(cl)), collapse = " & ")
            system(cmd, wait = FALSE, receive.console.signals = TRUE)
        }

        ## Accept connections and send the first command as initial
        ## handshake.  The handshake makes TCP synchronization detect and
        ## err on half-opened connections, which arise during parallel setup
        ## of client-server connections (due to internal timeouts, limited
        ## length of the listen backlog queue, race in timing out on
        ## creating a connection and probably more).
        ##
        ## The handshake looks like a regular server command followed by
        ## client response, which is compatible with older versions of R.

        cls <- if(useXDR) "SOCKnode" else "SOCK0node"
        ready <- 0
        pending <- list()
        on.exit(lapply(pending, function(x) close(x$con)), add = TRUE)
        t0 <- Sys.time()
        while (ready < length(cl)) {
            cons <- lapply(pending, function(x) x$con)

            if (difftime(Sys.time(), t0, units="secs") > setup_timeout + 5) {
                ## The workers will give up after setup_timeout, so there is
                ## no point waiting for them much longer.
                failed <- length(cl) - ready
                msg <- sprintf(ngettext(failed,
                           "Cluster setup failed. %d worker of %d failed to connect.",
                           "Cluster setup failed. %d of %d workers failed to connect."),
                               failed, length(cl))
                stop(msg)
            }
            a <- socketSelect(append(list(socket), cons), FALSE,
                              timeout = setup_timeout)
            canAccept <- a[1]
            canReceive <- seq_along(pending)[a[-1]]

            if (canAccept) {
                con <- socketAccept(socket = socket, blocking = TRUE,
                                    open = "a+b", timeout = timeout)
                scon <- structure(list(con = con, host = "localhost",
                                  rank = ready), class = cls)
                tryCatch({ sendCall(scon, eval, list(quote(Sys.getpid()))) },
                         error = identity)
                pending <- append(pending, list(scon))
            }
            for (scon in pending[canReceive]) {
                pid <- tryCatch({ recvResult(scon) }, error = identity)
                if (is.integer(pid)) {
                    ready <- ready + 1
                    cl[[ready]] <- scon
                } else
                    close(scon$con)
            }
            if (length(canReceive) > 0)
                pending <- pending[-canReceive]
        }
    } else {
        ## We could also use socketAccept() with sequential setup, but only
        ## if all workers connect to the same port.
        for (i in seq_along(cl))
            cl[[i]] <- newPSOCKnode(names[[i]], options = options, rank = i)
    }
    class(cl) <- c("SOCKcluster", "cluster")
    cl
}

print.SOCKcluster <- function(x, ...)
{
    nc <- length(x)
    hosts <- unique(sapply(x, `[[`, "host"))
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

stopCluster.SOCKcluster <- function(cl = NULL)
{
    for (n in cl) postNode(n, "DONE")

    ## Wait (with a timeout) for the worker connection to be closed by the
    ## OS, so that the cleanup of the worker's R session has a chance to run
    ## before stopCluster() finishes (PR#18133).

    t0 <- Sys.time()
    cleanup_timeout <- 5
    nodes <- cl
    while(length(nodes) > 0) {
        cons <- lapply(nodes, function(x) x$con)
        done <- socketSelect(cons, write = FALSE, timeout = cleanup_timeout)
        for(n in nodes[done]) closeNode(n)
        nodes <- nodes[!done]
        if (difftime(Sys.time(), t0, units="secs") > cleanup_timeout)
          break
    }

    ## Close the remaining worker connections unconditionally.
    for(n in nodes) closeNode(n)
}

.workRSOCK <- function()
{
    makeSOCKmaster <- function(master, port, setup_timeout, timeout, useXDR,
                               setup_strategy)
    {
        port <- as.integer(port)
        timeout <- as.integer(timeout)
        stopifnot(setup_timeout >= 0)
        cls <- if(useXDR) "SOCKnode" else "SOCK0node"

        ## Retry scheme parameters (do these need to be customizable?)
        retryDelay <- 0.1     # 0.1 second initial delay before retrying
        retryScale <- 1.5     # 50% increase of delay at each retry

        ## Retry multiple times in case the master is not yet ready
        t0 <- Sys.time()

        scon_timeout <- 1
        repeat {
            ## Set up a short timeout for the connection with parallel
            ## setup, which is needed to deal with half-opened connections
            ## (opened on client, closed on server).  The final connection
            ## timeout defaults to a large number, updated after the setup.
            if (setup_strategy == "parallel")
                scon_timeout <- scon_timeout + 0.2
            else
                ## Using "timeout" makes socketConnection() essentially
                ## blocking, which has been the practice for many years.
                ## Perhaps we could now use values similar to those for
                ## parallel setup.
                scon_timeout <- timeout

            con <- tryCatch({
                socketConnection(master, port = port, blocking = TRUE,
                                 open = "a+b",
                                 timeout = as.integer(scon_timeout))
            }, error = identity)

            hres <- NULL
            if (inherits(con, "connection")) {
                scon <- structure(list(con = con), class = cls)
                if (setup_strategy == "sequential")
                    return(scon)

                ## Serve the first command as a handshake during connection
                ## setup.  This is to get rid of half-opened connections.
                hres <- tryCatch({ workCommand(scon) }, error = identity)
                if (identical(hres, TRUE)) {
                    if (setup_strategy == "parallel")
                        socketTimeout(socket = con, timeout = timeout)
                    return(scon)
                } else if (identical(hres, FALSE)) {
                    ## "Done" command from server.  Could happen with server
                    ## accidentally not performing parallel setup.
                    return(NULL)
                } else
                    ## Error, possibly half-opened connection.
                    close(con)
            }

            if (difftime(Sys.time(), t0, units="secs") > setup_timeout) {
                if (inherits(hres, "error"))
                    stop(hres)
                if (inherits(con, "error"))
                    stop(con)
                stop("Connection setup failed or timed out.")
            }
            Sys.sleep(retryDelay)
            retryDelay <- retryScale * retryDelay
        }
    }

    ## set defaults in case run manually without args.
    master <- "localhost" # hostname of master process
    port <- NA_integer_   # no point in getting option on worker
    outfile <- Sys.getenv("R_SNOW_OUTFILE") # defaults to ""
    setup_timeout <- 120  # retry setup for 2 minutes before failing
    timeout <- 2592000L   # wait 30 days for new cmds before failing
    useXDR <- TRUE        # binary serialization
    setup_strategy <- "sequential"

    for (a in commandArgs(TRUE)) {
        ## Or use strsplit?
        pos <- regexpr("=", a)
        name <- substr(a, 1L, pos - 1L)
        value <- substr(a, pos + 1L, nchar(a))
        switch(name,
               MASTER = {master <- value},
               PORT = {port <- value},
               OUT = {outfile <- value},
               SETUPTIMEOUT = {setup_timeout <- as.numeric(value)},
               TIMEOUT = {timeout <- value},
               XDR = {useXDR <- as.logical(value)},
               SETUPSTRATEGY = {
                   setup_strategy <- match.arg(value,
                                               c("sequential", "parallel"))
               })
    }
    if (is.na(port)) stop("PORT must be specified")

    ## We should not need to attach parallel, as we are running in the namespace.

    sinkWorkerOutput(outfile)
    msg <- sprintf("starting worker pid=%d on %s at %s\n",
                   Sys.getpid(), paste(master, port, sep = ":"),
                   format(Sys.time(), "%H:%M:%OS3"))
    cat(msg)
    workLoop(makeSOCKmaster(master, port, setup_timeout, timeout, useXDR,
                            setup_strategy))
}
