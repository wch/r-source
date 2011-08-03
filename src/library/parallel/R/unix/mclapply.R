#  File src/library/parallel/R/unix/mclapply.R
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

mclapply <- function(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
                     mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                     mc.cleanup = TRUE)
{
    env <- parent.frame()
    cores <- as.integer(mc.cores)
    jobs <- list()
    cleanup <- function() {
        ## kill children if cleanup is requested
        if (length(jobs) && mc.cleanup) {
            ## first take care of uncollected children
            mccollect(children(jobs), FALSE)
            mckill(children(jobs),
                   if (is.integer(mc.cleanup)) mc.cleanup else SIGTERM)
            mccollect(children(jobs))
        }
        if (length(jobs)) {
            ## just in case there are zombies
            mccollect(children(jobs), FALSE)
        }
    }
    on.exit(cleanup())
    if (!mc.preschedule) {              # sequential (non-scheduled)
        FUN <- match.fun(FUN)
        if (length(X) <= cores) { # all-out, we can use one-shot parallel
            jobs <- lapply(seq(X),
                           function(i) mcparallel(FUN(X[[i]], ...),
                                                  name = names(X)[i],
                                                  mc.set.seed = mc.set.seed,
                                                  silent = mc.silent))
            res <- mccollect(jobs)
            if (length(res) == length(X)) names(res) <- names(X)
            return(res)
        } else { # more complicated, we have to wait for jobs selectively
            sx <- seq(X)
            res <- lapply(sx, function(x) NULL)
            names(res) <- names(X)
            ent <- rep(FALSE, length(X)) # values entered (scheduled)
            fin <- rep(FALSE, length(X)) # values finished
            jobid <- 1:cores
            jobs <- lapply(jobid,
                           function(i) mcparallel(FUN(X[[i]], ...),
                                                  mc.set.seed = mc.set.seed,
                                                  silent = mc.silent))
            jobsp <- processID(jobs)
            ent[jobid] <- TRUE
            while (!all(fin)) {
                s <- selectChildren(jobs, 0.5)
                if (is.null(s)) break   # no children -> no hope
                if (is.integer(s)) for (ch in s) {
                    ji <- which(jobsp == ch)[1]
                    ci <- jobid[ji]
                    r <- readChild(ch)
                    if (is.raw(r)) {
                        child.res <- unserialize(r)
                        ## we can't just assign it since a NULL
                        ## assignment would remove it from the list
                        if (!is.null(child.res)) res[[ci]] <- child.res
                    } else {
                        fin[ci] <- TRUE
                        if (!all(ent)) { # still something to do,
                                         # spawn a new job
                            nexti <- which(!ent)[1]
                            jobid[ji] <- nexti
                            jobs[[ji]] <- mcparallel(FUN(X[[nexti]], ...),
                                                     mc.set.seed = mc.set.seed,
                                                     silent = mc.silent)
                            jobsp[ji] <- processID(jobs[[ji]])
                            ent[nexti] <- TRUE
                        }
                    }
                }
            }
            return(res)
        }
    }
    if (length(X) < cores) cores <- length(X)
    if (cores < 2) return(lapply(X = X, FUN = FUN, ...))
    sindex <- lapply(seq_len(cores),
                     function(i) seq(i, length(X), by = cores))
    schedule <- lapply(seq_len(cores),
                       function(i) X[seq(i, length(X), by = cores)])
    ch <- list()
    res <- lapply(seq(X), function(x) NULL)
    names(res) <- names(X)
    cp <- rep(0L, cores)
    fin <- rep(FALSE, cores)
    dr <- rep(FALSE, cores)
    inner.do <- function(core) {
        S <- schedule[[core]]
        f <- mcfork()
        if (inherits(f, "masterProcess")) { # this is the child process
            on.exit(mcexit(1L, structure("fatal error in wrapper code", class="try-error")))
            if (isTRUE(mc.set.seed)) set.seed(Sys.getpid())
            if (isTRUE(mc.silent)) closeStdout()
            sendMaster(try(lapply(X = S, FUN = FUN, ...), silent = TRUE))
            mcexit(0L)
        }
        jobs[[core]] <<- ch[[core]] <<- f
        cp[core] <<- f$pid
        NULL
    }
    lapply(seq_len(cores), inner.do)
    ac <- cp[cp > 0]
    while (!all(fin)) {
        s <- selectChildren(ac, 1)
        if (is.null(s)) break # no children -> no hope we get anything
        if (is.integer(s)) for (ch in s) {
            a <- readChild(ch)
            if (is.integer(a)) {
                core <- which(cp == a)
                fin[core] <- TRUE
            } else if (is.raw(a)) {
                core <- which(cp == attr(a, "pid"))
                res[[core]] <- unserialize(a)
                dr[core] <- TRUE
            }
        }
    }
    ores <- list()
    for (i in seq_len(cores)) ores[sindex[[i]]] <- res[[i]]
    if (length(names(X)) == length(ores)) names(ores) <- names(X)
    ores
}
