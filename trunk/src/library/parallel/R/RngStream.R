#  File src/library/parallel/R/RngStream.R
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

nextRNGStream <- function(seed)
{
    if(!is.integer(seed) || seed[1L] %% 100L != 7L)
	stop(gettextf("invalid value of %s", "'seed'"), domain = NA)
    .Call(C_nextStream, seed)
}

nextRNGSubStream <- function(seed)
{
    if(!is.integer(seed) || seed[1L] %% 100L != 7L)
	stop(gettextf("invalid value of %s", "'seed'"), domain = NA)
    .Call(C_nextSubStream, seed)
}

## Different from snow's RNG code
clusterSetRNGStream <- function(cl = NULL, iseed = NULL)
{
    cl <- defaultCluster(cl)
    oldseed <-
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
        else NULL
    RNGkind("L'Ecuyer-CMRG")
    if(!is.null(iseed)) set.seed(iseed)
    nc <- length(cl)
    seeds <- vector("list", nc)
    seeds[[1L]] <- .Random.seed
    for(i in seq_len(nc-1L)) seeds[[i+1L]] <- nextRNGStream(seeds[[i]])
    ## Reset the random seed in the master.
    if(!is.null(oldseed))
        assign(".Random.seed", oldseed, envir = .GlobalEnv)
    else rm(.Random.seed, envir = .GlobalEnv)
    for (i in seq_along(cl)) {
        expr <- substitute(assign(".Random.seed", seed, envir = .GlobalEnv),
                           list(seed = seeds[[i]]))
        sendCall(cl[[i]], eval, list(expr))
    }
    checkForRemoteErrors(lapply(cl, recvResult))
    invisible()
}

RNGenv <- new.env()

mc.reset.stream <- function() {
    if (RNGkind()[1L] == "L'Ecuyer-CMRG") {
        if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            sample.int(1L)
        assign("LEcuyer.seed",
               get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
               envir = RNGenv)
    }
}

## For use in the master before forking
mc.advance.stream <- function(reset = FALSE)
{
    if (RNGkind()[1L] == "L'Ecuyer-CMRG") {
        if (reset ||
            !exists("LEcuyer.seed", envir = RNGenv, inherits = FALSE)) {
            if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
                sample.int(1L)
            assign("LEcuyer.seed",
                   get(".Random.seed", envir = .GlobalEnv, inherits = FALSE),
                   envir = RNGenv)
        } else {
            assign("LEcuyer.seed",
                   nextRNGStream(get("LEcuyer.seed", envir = RNGenv)),
                   envir = RNGenv)
        }
    }
}

## For use in the child
mc.set.stream <- function()
{
    if (RNGkind()[1L] == "L'Ecuyer-CMRG") {
            assign(".Random.seed", get("LEcuyer.seed", envir = RNGenv),
                   envir = .GlobalEnv)
    } else {
        ## It is random to simply unset the seed
        if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE))
            rm(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    }
}
