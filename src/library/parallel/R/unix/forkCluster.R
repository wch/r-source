#  File src/library/parallel/R/unix/forkCluster.R
#  Part of the R package, http://www.R-project.org
#
#  Copyright (C) 1995-2012 The R Core Team
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

makeForkCluster <- function(nnodes = getOption("mc.cores", 2L), ...)
{
    if(nnodes < 1L) stop("'nnodes' must be >= 1")
    cl <- vector("list", nnodes)
    for (i in seq_along(cl)) cl[[i]] <- newForkNode(..., rank = i)
    class(cl) <- c("SOCKcluster", "cluster")
    cl
}


newForkNode <- function(..., options = defaultClusterOptions, rank)
{
    options <- addClusterOptions(options, list(...))
    outfile <- getClusterOption("outfile", options)
    port <- getClusterOption("port", options)
    timeout <- getClusterOption("timeout", options)
    renice <- getClusterOption("renice", options)

    f <- mcfork()
    if (inherits(f, "masterProcess")) { # the slave
        on.exit(mcexit(1L, structure("fatal error in wrapper code",
                                  class = "try-error")))
        # closeStdout()
        master <- "localhost"
        makeSOCKmaster <- function(master, port, timeout)
        {
            port <- as.integer(port)
            ## maybe use `try' and sleep/retry if first time fails?
            con <- socketConnection(master, port = port, blocking = TRUE,
                                    open = "a+b", timeout = timeout)
            structure(list(con = con), class = "SOCK0node")
        }
        sinkWorkerOutput(outfile)
        msg <- sprintf("starting worker pid=%d on %s at %s\n",
                       Sys.getpid(), paste(master, port, sep = ":"),
                       format(Sys.time(), "%H:%M:%OS3"))
        cat(msg)
        ## allow this to quit when the loop is done.
        tools::pskill(Sys.getpid(), tools::SIGUSR1)
        if(!is.na(renice) && renice) ## ignore 0
            tools::psnice(Sys.getpid(), renice)
        slaveLoop(makeSOCKmaster(master, port, timeout))
        mcexit(0L)
    }

    con <- socketConnection("localhost", port = port, server = TRUE,
                            blocking = TRUE, open = "a+b", timeout = timeout)
    structure(list(con = con, host = "localhost", rank = rank),
              class = c("forknode", "SOCK0node"))
}
