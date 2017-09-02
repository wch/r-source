#  File src/library/parallel/R/unix/mclapply.R
#  Part of the R package, https://www.R-project.org
#
#  Copyright (C) 1995-2017 The R Core Team
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

### Derived from multicore version 0.1-6 by Simon Urbanek

mclapply <- function(X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE,
                     mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L),
                     mc.cleanup = TRUE, mc.allow.recursive = TRUE,
                     affinity.list = NULL)
{
    cores <- as.integer(mc.cores)
    if((is.na(cores) || cores < 1L) && is.null(affinity.list))
        stop("'mc.cores' must be >= 1")
    .check_ncores(cores)

    if (isChild() && !isTRUE(mc.allow.recursive))
        return(lapply(X = X, FUN = FUN, ...))

    if(mc.set.seed) mc.reset.stream()
    if(length(X) < 2) {
        old.aff <- mcaffinity()
        mcaffinity(affinity.list[[1]])
        res <- lapply(X = X, FUN = FUN, ...)
        mcaffinity(old.aff)
        return(res)
    }

    jobs <- list()
    cleanup <- function() {
        ## kill children if cleanup is requested
        if (length(jobs) && mc.cleanup) {
            ## first take care of uncollected children
            mccollect(children(jobs), FALSE)
            mckill(children(jobs),
                   if (is.integer(mc.cleanup)) mc.cleanup else tools::SIGTERM)
            mccollect(children(jobs))
        }
        if (length(jobs)) {
            ## just in case there are zombies
            mccollect(children(jobs), FALSE)
        }
    }
    on.exit(cleanup())
    ## Follow lapply
    if(!is.vector(X) || is.object(X)) X <- as.list(X)
    if(!is.null(affinity.list) && length(affinity.list) < length(X))
        stop("affinity.list and X must have the same length")
    if (!mc.preschedule) {              # sequential (non-scheduled)
        FUN <- match.fun(FUN)
        if (length(X) <= cores && is.null(affinity.list)) { # we can use one-shot parallel
            jobs <- lapply(seq_along(X),
                           function(i) mcparallel(FUN(X[[i]], ...),
                                                  name = names(X)[i],
                                                  mc.set.seed = mc.set.seed,
                                                  silent = mc.silent))
            res <- mccollect(jobs)
            if (length(res) == length(X)) names(res) <- names(X)
            has.errors <- sum(sapply(res, inherits, "try-error"))
        } else { # more complicated, we have to wait for jobs selectively
            sx <- seq_along(X)
            res <- vector("list", length(sx))
            names(res) <- names(X)
            fin <- rep(FALSE, length(X)) # values finished
            if (!is.null(affinity.list)) {
                ## build matrix for job mapping with affinity.list
                ## entry i,j is true if item i is allowed to run on core j
                cores <- max(unlist(x = affinity.list, recursive = TRUE))
                cpu.map <- lapply(sx, function (i){
                    data <- vector(mode = "logical", length = cores)
                    data[as.vector(affinity.list[[i]])] <- TRUE
                    data
                })
                ava <- do.call(rbind, cpu.map)
            } else {
                ## build matrix for job mapping without affinity.list
                ## all entries true
                ava <- matrix(TRUE, nrow = length(X), ncol = cores)
            }
            jobid <- numeric(cores)
            ## choose first job for each core to start
            for (i in 1:cores) {
                jobid[i] <- which(ava[, i])[1]
                ava[jobid[i],] <- FALSE
            }
            ## remove unused cores from matrix
            if(anyNA(jobid)) {
                unusedCores <- which(is.na(jobid))
                jobid <- na.omit(jobid)
                ava <- ava[, -unusedCores, drop = FALSE]
            }
            jobs <- lapply(jobid,
                           function(i) mcparallel(FUN(X[[i]], ...),
                                                  mc.set.seed = mc.set.seed,
                                                  silent = mc.silent,
                                                  mc.affinity = affinity.list[[i]]))
            jobsp <- processID(jobs)
            has.errors <- 0L
            while (!all(fin)) {
                s <- selectChildren(jobs, 0.5)
                if (is.null(s)) break   # no children -> no hope
                if (is.integer(s))
                    for (ch in s) {
                        ji <- which(jobsp == ch)[1]
                        ci <- jobid[ji]
                        r <- readChild(ch)
                        if (is.raw(r)) {
                            child.res <- unserialize(r)
                            if (inherits(child.res, "try-error"))
                                has.errors <- has.errors + 1L
                            ## we can't just assign it since a NULL
                            ## assignment would remove it from the list
                            if (!is.null(child.res)) res[[ci]] <- child.res
                        } else {
                            fin[ci] <- TRUE
                            if (any(ava)) { # still something to do,
                                ## look for first job which is allowed to
                                ## run on the now idling core and spawn it
                                nexti <- which.max(ava[, ji])
                                if(!is.na(nexti)) {
                                    jobid[ji] <- nexti
                                    jobs[[ji]] <- mcparallel(FUN(X[[nexti]], ...),
                                                             mc.set.seed = mc.set.seed,
                                                             silent = mc.silent,
                                                             mc.affinity = affinity.list[[nexti]])
                                    jobsp[ji] <- processID(jobs[[ji]])
                                    ava[nexti,] <- FALSE
                                }
                            }
                        }
                    }
            }
        }
        if (has.errors)
            warning(gettextf("%d function calls resulted in an error",
                             has.errors), domain = NA)
        return(res)
    }

    ## mc.preschedule = TRUE from here on.
    if(!is.null(affinity.list))
        warning("'mc.preschedule' must be false if 'affinity.list' is used")
    if (length(X) < cores) cores <- length(X)
    if (cores < 2L) return(lapply(X = X, FUN = FUN, ...))
    sindex <- lapply(seq_len(cores),
                     function(i) seq(i, length(X), by = cores))
    schedule <- lapply(seq_len(cores),
                       function(i) X[seq(i, length(X), by = cores)])
    ch <- list()
    res <- vector("list", length(X))
    names(res) <- names(X)
    cp <- rep(0L, cores)
    fin <- rep(FALSE, cores)
    dr <- rep(FALSE, cores)
    inner.do <- function(core) {
        S <- schedule[[core]]
        f <- mcfork()
        if (isTRUE(mc.set.seed)) mc.advance.stream()
        if (inherits(f, "masterProcess")) { # this is the child process
            on.exit(mcexit(1L, structure("fatal error in wrapper code", class="try-error")))
            if (isTRUE(mc.set.seed)) mc.set.stream()
            if (isTRUE(mc.silent)) closeStdout(TRUE)
            sendMaster(try(lapply(X = S, FUN = FUN, ...), silent = TRUE))
            mcexit(0L)
        }
        jobs[[core]] <<- ch[[core]] <<- f
        cp[core] <<- f$pid
        NULL
    }
    job.res <- lapply(seq_len(cores), inner.do)
    ac <- cp[cp > 0]
    has.errors <- integer(0)
    while (!all(fin)) {
        s <- selectChildren(ac, 1)
        if (is.null(s)) break # no children -> no hope we get anything
        if (is.integer(s))
            for (ch in s) {
                a <- readChild(ch)
                if (is.integer(a)) {
                    core <- which(cp == a)
                    fin[core] <- TRUE
                } else if (is.raw(a)) {
                    core <- which(cp == attr(a, "pid"))
                    job.res[[core]] <- ijr <- unserialize(a)
                    if (inherits(ijr, "try-error"))
                        has.errors <- c(has.errors, core)
                    dr[core] <- TRUE
                }
            }
    }
    for (i in seq_len(cores)) {
        this <- job.res[[i]]
        if (inherits(this, "try-error")) { ## length-1 result
            for (j in sindex[[i]]) res[[j]] <- this
        } else res[sindex[[i]]] <- this
    }
    if (length(has.errors)) {
        if (length(has.errors) == cores)
            warning("all scheduled cores encountered errors in user code")
        else
            warning(sprintf(ngettext(has.errors,
                                     "scheduled core %s encountered error in user code, all values of the job will be affected",
                                     "scheduled cores %s encountered errors in user code, all values of the jobs will be affected"),
                            paste(has.errors, collapse = ", ")),
                    domain = NA)
    }
    res
}
