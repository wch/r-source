#  File src/library/parallel/R/unix/forkCluster.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2020 The R Core Team
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

makeForkCluster <- function(nnodes = getOption("mc.cores", 2L), ...)
{
    options <- addClusterOptions(defaultClusterOptions, list(...))
    port <- getClusterOption("port", options)

    nnodes <- as.integer(nnodes)
    if(is.na(nnodes) || nnodes < 1L) stop("'nnodes' must be >= 1")
    .check_ncores(nnodes)
    cl <- vector("list", nnodes)
    socket <- serverSocket(port = port)
    on.exit(close(socket))
    for (i in seq_along(cl)) {
        node <- tryCatch( newForkNode(..., rank = i, socket = socket,
                                      server_socket = socket),
                          error = identity )
        if (inherits(node, "forknode"))
            cl[[i]] <- node
        else {
            if (i > 1)
                for(j in 1:i) stopNode(cl[[j]])
            stop("Cluster setup failed.")
        }
    }
    class(cl) <- c("SOCKcluster", "cluster")
    cl
}


newForkNode <- function(..., options = defaultClusterOptions, rank, socket,
                        setup_timeout, server_socket)
{
    options <- addClusterOptions(options, list(...))
    outfile <- getClusterOption("outfile", options)
    port <- getClusterOption("port", options)
    timeout <- getClusterOption("timeout", options)
    renice <- getClusterOption("renice", options)
    setup_timeout <- 10

    f <- mcfork(TRUE)
    if (inherits(f, "masterProcess")) { # the worker
        on.exit(mcexit(1L, structure("fatal error in wrapper code",
                                  class = "try-error")))
        # closeStdout()
        close(server_socket)
        master <- "localhost"
        makeSOCKmaster <- function(master, port, timeout)
        {
            port <- as.integer(port)
            con <- socketConnection(master, port = port, blocking = TRUE,
                                    open = "a+b", timeout = setup_timeout)
            socketTimeout(socket = con, timeout = timeout)
            structure(list(con = con), class = "SOCK0node")
        }
        sinkWorkerOutput(outfile)
        msg <- sprintf("starting worker pid=%d on %s at %s\n",
                       Sys.getpid(), paste(master, port, sep = ":"),
                       format(Sys.time(), "%H:%M:%OS3"))
        cat(msg)
        if(!is.na(renice) && renice) ## ignore 0
            tools::psnice(Sys.getpid(), renice)
        workLoop(makeSOCKmaster(master, port, timeout))
        mcexit(0L)
    }


    con <- socketAccept(socket = socket, blocking = TRUE, open = "a+b",
                        timeout = setup_timeout + 5)
    socketTimeout(socket = con, timeout = timeout)
    structure(list(con = con, host = "localhost", rank = rank),
              class = c("forknode", "SOCK0node"))
}
