#  File src/library/parallel/R/multicore.R
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

# extracted from multicore 0.1-5

forkR <- function()
{
    r <- .Call("mc_fork", PACKAGE = "parallel")
    structure(list(pid = r[1], fd = r[2:3]),
              class = c(ifelse(r[1] != 0L, "childProcess", "masterProcess"),
                        "process"))
}

sendRmaster <- function(what)
{
    if (!is.raw(what)) what <- serialize(what, NULL, FALSE)
    .Call("mc_send_master", what, PACKAGE = "parallel")
}

processID <- function (process) {
    if (inherits(process, "process")) process$pid
    else if (is.list(process)) unlist(lapply(process, processID))
    else stop("process must be of the class `process'")
}

selectChildren <- function (children = NULL, timeout = 0)
{
    if (!length(children)) children <- integer()
    if (inherits(children, "process")) children <- processID(children)
    if (is.list(children))
        children <- unlist(lapply(children, function(x)
                                  if (inherits(x, "process")) x$pid
                                  else stop("'children' must be a list of processes or a single process")))
    if (!is.numeric(children))
        stop("'children' must be a list of processes or a single process")
    .Call("mc_select_children", as.double(timeout), as.integer(children),
          PACKAGE = "paralllel")
}

readChild <- function (child)
{
    if (inherits(child, "process")) child <- processID(child)
    if (!is.numeric(child)) stop("invalid 'child' argument")
    .Call("mc_read_child", as.integer(child), PACKAGE = "parallel")
}

children <- function (select)
{
    p <- .Call("mc_children", PACKAGE = "parallel")
    if (!missing(select))
        p <- p[p %in% processID(select)]
    lapply(p, function(x) structure(list(pid = x),
                                    class = c("childProcess", "process")))
}

parallel <- function (expr, mc.set.seed, silent)
{
    exit <- function(status)
        .Call("mc_exit", status, PACKAGE = "parallel")
    f <- forkR()
    env <- parent.frame()
    if (inherits(f, "masterProcess")) {
        on.exit(exit(1L, structure("fatal error in wrapper code",
                                  class = "try-error")))
        sendRmaster(try(eval(expr, env), silent = TRUE))
        exit(0L)
    }
    class(f) <- c("parallelJob", class(f))
    f
}

collect <- function(jobs, wait = TRUE, timeout = 0, intermediate = FALSE)
{
    if (missing(jobs)) jobs <- children()
    if (!length(jobs)) return (NULL)
    if (isTRUE(intermediate)) intermediate <- str
    if (!wait) {
        s <- selectChildren(jobs, timeout)
        if (is.logical(s) || !length(s)) return(NULL)
        lapply(s, function(x) {
            r <- readChild(x)
            if (is.raw(r)) unserialize(r) else NULL
        })
    } else {
        pids <- if (inherits(jobs, "process") || is.list(jobs)) processID(jobs) else jobs
        if (!length(pids)) return(NULL)
        if (!is.numeric(pids)) stop("invalid 'jobs' argument")
        pids <- as.integer(pids)
        pnames <- as.character(pids)
        if (!inherits(jobs, "process") && is.list(jobs))
            for(i in seq(jobs))
                if (!is.null(jobs[[i]]$name))
                    pnames[i] <- as.character(jobs[[i]]$name)
        res <- lapply(pids, function(x) NULL)
        names(res) <- pnames
        fin <- rep(FALSE, length(jobs))
        while (!all(fin)) {
            s <- selectChildren(pids, 0.5)
            if (is.integer(s)) {
                for (pid in s) {
                    r <- readChild(pid)
                    if (is.integer(r) || is.null(r)) fin[pid == pids] <- TRUE
                    if (is.raw(r)) res[[which(pid == pids)]] <- unserialize(r)
                }
                if (is.function(intermediate)) intermediate(res)
            } else if (all(is.na(match(pids, processID(children()))))) break
        }
        res
    }
}
