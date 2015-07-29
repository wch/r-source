#  File src/library/parallel/R/unix/mcparallel.R
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

mcaffinity <- function(affinity = NULL) .Call(C_mc_affinity, affinity)

mcparallel <- function(expr, name, mc.set.seed = TRUE, silent = FALSE, mc.affinity = NULL, mc.interactive = FALSE, detached = FALSE)
{
    f <- mcfork(detached)
    env <- parent.frame()
    if (isTRUE(mc.set.seed)) mc.advance.stream()
    if (inherits(f, "masterProcess")) {
        on.exit(mcexit(1L, structure("fatal error in wrapper code",
                                  class = "try-error")))
        if (isTRUE(mc.set.seed)) mc.set.stream()
        mc.interactive <- as.logical(mc.interactive)
        if (isTRUE(mc.interactive)) .Call(C_mc_interactive, TRUE)
        if (isTRUE(!mc.interactive)) .Call(C_mc_interactive, FALSE)
        if (!is.null(mc.affinity)) .Call(C_mc_affinity, mc.affinity)
        if (isTRUE(silent)) closeStdout(TRUE)
	if (detached) {
	    on.exit(mcexit(1L))
	    eval(expr, env)
	    mcexit(0L)
	}
	sendMaster(try(eval(expr, env), silent = TRUE))
        mcexit(0L)
    }
    if (!missing(name) && !is.null(name)) f$name <- as.character(name)[1L]
    class(f) <- c("parallelJob", class(f))
    f
}

mccollect <- function(jobs, wait = TRUE, timeout = 0, intermediate = FALSE)
{
    if (missing(jobs)) jobs <- children()
    if (!length(jobs)) return (NULL)
    if (isTRUE(intermediate)) intermediate <- utils::str
    if (!wait) {
        s <- selectChildren(jobs, timeout)
        if (is.logical(s) || !length(s)) return(NULL)
        lapply(s, function(x) {
            r <- readChild(x)
            if (is.raw(r)) unserialize(r) else NULL
        })
    } else {
        pids <- if (inherits(jobs, "process") || is.list(jobs))
            processID(jobs) else jobs
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
                    if (is.raw(r)) # unserialize(r) might be null
                        res[which(pid == pids)] <- list(unserialize(r))
                }
                if (is.function(intermediate)) intermediate(res)
            } else if (all(is.na(match(pids, processID(children()))))) break
        }
        res
    }
}
