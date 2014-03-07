#  File src/library/parallel/R/unix/mcfork.R
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

### Derived from multicore version 0.1-6 by Simon Urbanek

### --- multicore --- low-level functions ---

## all not exported in parallel.

## FIXME: mc_pids doesn't really work, because children stay in the
## pid list even after they die, so innocent processes may
## may get killed instead!
mc_pids <- new.env()
assign("pids", integer(), envir = mc_pids)
clean_pids <- function(e)
    if(length(pids <- get("pids", envir = e))) tools::pskill(pids, tools::SIGKILL)

mcfork <- function(estranged = FALSE) {
    r <- .Call(C_mc_fork, estranged)
    processClass <- if (!r[1L]) "masterProcess" else
    		    if (is.na(r[2L])) "estrangedProcess" else
        {
	    assign("pids", c(get("pids",envir = mc_pids), r[1L]), envir = mc_pids)
	    "childProcess"
        }
    structure(list(pid = r[1L], fd = r[2:3]), class = c(processClass, "process"))
}

## not used
readChildren <- function(timeout = 0)
    .Call(C_mc_read_children, as.double(timeout))

## used by mccollect, mclapply
readChild <- function(child)
{
    if (inherits(child, "process")) child <- processID(child)
    if (!is.numeric(child)) stop("invalid 'child' argument")
    .Call(C_mc_read_child, as.integer(child))
}

## used by mccollect, mclapply
selectChildren <- function(children = NULL, timeout = 0)
{
    if (!length(children)) children <- integer()
    if (inherits(children, "process")) children <- processID(children)
    if (is.list(children))
        children <- unlist(lapply(children, function(x) if (inherits(x, "process")) x$pid
        else stop("'children' must be a list of processes or a single process")))
    if (!is.numeric(children))
        stop("'children' must be a list of processes or a single process")
    .Call(C_mc_select_children, as.double(timeout), as.integer(children))
}

rmChild <- function(child)
{
    if (inherits(child, "process")) child <- processID(child)
    if (!is.numeric(child)) stop("invalid 'child' argument")
    .Call(C_mc_rm_child, as.integer(child))
}

## used in pvec, mclapply
mckill <- function(process, signal = 2L)
{
    process <- processID(process)
    ## or simply tools::pskill(process, signal)
    unlist(lapply(process, function(p)
                  .Call(C_mc_kill, as.integer(p), as.integer(signal))))
}

## used by mcparallel, mclapply
sendMaster <- function(what)
{
    # This is talking to the same machine, so no point in using xdr.
    if (!is.raw(what)) what <- serialize(what, NULL, xdr = FALSE)
    .Call(C_mc_send_master, what)
}

processID <- function(process) {
    if (inherits(process, "process")) process$pid
    else if (is.list(process)) unlist(lapply(process, processID))
    else stop(gettextf("'process' must be of class %s", dQuote("process")),
              domain = NA)
}

# unused
sendChildStdin <- function(child, what)
{
    if (inherits(child, "process") || is.list(child)) child <- processID(child)
    if (!is.numeric(child) || !length(child))
        stop("'child' must be a valid child process")
    child <- as.integer(child)
    if (is.character(what)) what <- charToRaw(paste(what, collapse='\n'))
    if (!is.raw(what)) stop("'what' must be a character or raw vector")
    invisible(unlist(lapply(child, function(p)
                            .Call(C_mc_send_child_stdin, p, what))))
}

## used by mcparallel, mclapply
mcexit <- function(exit.code = 0L, send = NULL)
{
    if (!is.null(send)) try(sendMaster(send), silent = TRUE)
    .Call(C_mc_exit, as.integer(exit.code))
}

## used by mccollect, mclapply
children <- function(select)
{
    p <- .Call(C_mc_children)
    if (!missing(select)) p <- p[p %in% processID(select)]
    ## FIXME: this is not the meaning of this class as returned by mcfork
    lapply(p, function(x)
           structure(list(pid = x), class = c("childProcess", "process")))
}

childrenDescriptors <- function(index = 0L)
    .Call(C_mc_fds, as.integer(index))

masterDescriptor <- function() .Call(C_mc_master_fd)

isChild <- function() .Call(C_mc_is_child)

closeStdout <- function(to.null=FALSE) .Call(C_mc_close_stdout, to.null)
closeStderr <- function(to.null=FALSE) .Call(C_mc_close_stderr, to.null)
closeFD <- function(fds) .Call(C_mc_close_fds, as.integer(fds))

closeAll <- function(includeStd = FALSE)
{
    if (!isChild()) {
        warning("closeAll() is a no-op in the master process", domain = NA)
        return(invisible(FALSE))
    }
    fds <- masterDescriptor()
    if (identical(fds, -1L)) fds <- integer(0)
    if (includeStd) fds <- c(1L, 2L, fds)
    mf <- max(fds) + 16L # take a few more ...
    ## close all but those that we actually use
    closeFD((1:mf)[-fds])
}
